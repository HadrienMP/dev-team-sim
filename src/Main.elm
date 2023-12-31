module Main exposing (..)

import Browser
import Calendar exposing (Calendar)
import Character exposing (Character)
import Core.DevTask as DevTask exposing (Task)
import Core.Settings as Settings
import Core.Throughput as Throughput
import Dict exposing (update)
import Html exposing (label)
import Html.Attributes
import Lib.Duration as Duration exposing (Duration(..))
import Platform.Cmd as Cmd
import Process
import Random
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- init


type alias Flags =
    ()


type alias TaskProgress =
    { size : Int
    , done : Int
    }


type SimTime
    = Slow
    | Medium
    | Fast


type alias Model =
    { todo : List Task
    , done : List Task
    , calendar : Calendar
    , character : Maybe Character.Character
    , simTime : SimTime
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { todo = []
      , done = []
      , calendar = Calendar.init
      , character = Nothing
      , simTime = Slow
      }
    , Cmd.batch
        [ DevTask.random |> Random.list 20 |> Random.generate GotTasks
        , Character.random |> Random.generate GotCharacter
        ]
    )



-- update


type Msg
    = GotTasks (List Task)
    | GotCharacter Character.Character
    | StartNextTask Character.Id
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTasks tasks ->
            ( { model | todo = tasks }, Cmd.none )

        GotCharacter character ->
            ( { model | character = Just character }
            , Task.succeed ()
                |> Task.perform (always (StartNextTask character.id))
            )

        StartNextTask characterId ->
            case model.todo of
                first :: rest ->
                    ( { model
                        | todo = rest
                        , character =
                            model.character
                                |> Maybe.map
                                    (\character ->
                                        { character
                                            | task =
                                                Just
                                                    { size = Duration.toHours first.size |> round
                                                    , done = 0
                                                    }
                                        }
                                    )
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        Tick ->
            model.character
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
                |> advanceCharacters model
                |> (\( character, done, command ) ->
                        ( { model
                            | character = List.head character
                            , done = done ++ model.done
                            , calendar = Calendar.addHour model.calendar
                          }
                        , command
                        )
                   )


advanceCharacters : Model -> List Character -> ( List Character, List Task, Cmd Msg )
advanceCharacters model characters =
    advanceCharactersRec model
        { toAdvance = characters
        , advanced = []
        , done = []
        , cmd = Cmd.none
        }


advanceCharactersRec :
    Model
    ->
        { toAdvance : List Character
        , advanced : List Character
        , done : List Task
        , cmd : Cmd Msg
        }
    -> ( List Character, List Task, Cmd Msg )
advanceCharactersRec model { toAdvance, advanced, done, cmd } =
    case toAdvance of
        [] ->
            ( advanced, done, cmd )

        character :: others ->
            let
                ( next, newDone, nextCmd ) =
                    advanceCharacter model character
            in
            advanceCharactersRec model
                { toAdvance = others
                , advanced = next :: advanced
                , done = newDone |> Maybe.map (\a -> a :: done) |> Maybe.withDefault done
                , cmd = Cmd.batch [ nextCmd, cmd ]
                }


advanceCharacter : Model -> Character -> ( Character, Maybe Task, Cmd Msg )
advanceCharacter model character =
    case character.task of
        Nothing ->
            ( character, Nothing, Cmd.none )

        Just task ->
            let
                nextTask =
                    { task | done = task.done + 1 }
            in
            if nextTask.done == nextTask.size then
                ( { character | task = Nothing }
                , Just { size = Hour <| toFloat task.size }
                , Settings.default.breakDuration
                    |> toSimTime model.simTime
                    |> Process.sleep
                    |> Task.perform (always (StartNextTask character.id))
                )

            else
                ( { character | task = Just nextTask }
                , Nothing
                , Cmd.none
                )


toSimTime : SimTime -> Duration -> Float
toSimTime simTime duration =
    Duration.toHours duration
        * (case simTime of
            Slow ->
                500

            Medium ->
                300

            Fast ->
                100
          )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Hour 1 |> toSimTime model.simTime) (always Tick)



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Dev Team Sim"
    , body =
        [ Html.main_ []
            [ Html.section [ Html.Attributes.id "sim" ]
                [ Html.section
                    [ Html.Attributes.class "task-list" ]
                    (Html.h2 [] [ Html.text "Todo" ] :: (model.todo |> List.map viewTask))
                , model.character
                    |> Maybe.map Character.view
                    |> Maybe.withDefault (Html.text "")
                , Html.section
                    [ Html.Attributes.class "task-list" ]
                    (Html.h2 [] [ Html.text "Done" ] :: (model.done |> List.map viewTask))
                ]
            , viewStatistics model
            ]
        ]
    }


viewStatistics : Model -> Html.Html Msg
viewStatistics model =
    let
        throughput =
            Throughput.fromList model.done Settings.default
    in
    Html.section [ Html.Attributes.id "statistics" ]
        [ Html.h2 [] [ Html.text "Statistics" ]
        , Html.text <| Calendar.print model.calendar
        , viewMetric { label = "WIP", value = "1" }
        , viewMetric { label = "Throughput", value = throughput |> Throughput.print }
        , viewMetric { label = "Lead Time", value = throughput |> Throughput.leadTime { wip = 1, settings = Settings.default } |> Duration.print }
        , viewMetric
            { label = "Mean task duration"
            , value =
                model.done
                    |> List.map .size
                    |> List.foldl Duration.add Duration.zero
                    |> Duration.divideByInt (List.length model.done)
                    |> Duration.print
            }
        , viewMetric { label = "Cost", value = "75 $/h" }
        ]


viewMetric : { label : String, value : String } -> Html.Html Msg
viewMetric { label, value } =
    Html.div [ Html.Attributes.class "metric" ]
        [ Html.span [ Html.Attributes.class "label" ] [ Html.text label ]
        , Html.span [ Html.Attributes.class "value" ] [ Html.text value ]
        ]


viewTask : Task -> Html.Html Msg
viewTask task =
    Html.div [ Html.Attributes.class "task" ] [ Html.text <| Duration.print task.size ]
