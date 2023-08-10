module Main exposing (..)

import Browser
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


type alias Model =
    { todo : List Task
    , done : List Task
    , character : Maybe Character.Character
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { todo = []
      , done = []
      , character = Nothing
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
    | AdvanceTask Character.Id


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
                    , Hour 1
                        |> toSimTime
                        |> Process.sleep
                        |> Task.perform (always (AdvanceTask characterId))
                    )

                [] ->
                    ( model, Cmd.none )

        AdvanceTask _ ->
            model.character
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
                |> advanceCharacters
                |> (\( character, done, command ) ->
                        ( { model
                            | character = List.head character
                            , done = done ++ model.done
                          }
                        , command
                        )
                   )


advanceCharacters : List Character -> ( List Character, List Task, Cmd Msg )
advanceCharacters characters =
    advanceCharactersRec
        { toAdvance = characters
        , advanced = []
        , done = []
        , cmd = Cmd.none
        }


advanceCharactersRec :
    { toAdvance : List Character
    , advanced : List Character
    , done : List Task
    , cmd : Cmd Msg
    }
    -> ( List Character, List Task, Cmd Msg )
advanceCharactersRec { toAdvance, advanced, done, cmd } =
    case toAdvance of
        [] ->
            ( advanced, done, cmd )

        character :: others ->
            let
                ( next, newDone, nextCmd ) =
                    advanceCharacter character
            in
            advanceCharactersRec
                { toAdvance = others
                , advanced = next :: advanced
                , done = newDone |> Maybe.map (\a -> a :: done) |> Maybe.withDefault done
                , cmd = Cmd.batch [ nextCmd, cmd ]
                }


advanceCharacter : Character -> ( Character, Maybe Task, Cmd Msg )
advanceCharacter character =
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
                    |> toSimTime
                    |> Process.sleep
                    |> Task.perform (always (StartNextTask character.id))
                )

            else
                ( { character | task = Just nextTask }
                , Nothing
                , Hour 1
                    |> toSimTime
                    |> Process.sleep
                    |> Task.perform (always (AdvanceTask character.id))
                )


toSimTime : Duration -> Float
toSimTime duration =
    Duration.toHours duration * 100



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
