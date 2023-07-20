module Main exposing (..)

import Browser
import DevTask exposing (Task)
import Dict exposing (update)
import Duration exposing (Duration(..))
import Html exposing (label)
import Html.Attributes
import Platform.Cmd as Cmd
import Random
import Settings exposing (Settings)
import Throughput
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


settings : Settings
settings =
    { dayLength = Hour 8, breakDuration = Hour 1 }


type alias Flags =
    ()


type alias TaskProgress =
    { size : Int
    , done : Int
    }


type alias Model =
    { current : Maybe TaskProgress
    , todo : List Task
    , done : List Task
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { current = Nothing
      , todo = []
      , done = []
      }
    , DevTask.random |> Random.list 20 |> Random.generate GotTasks
    )



-- update


type Msg
    = GotTasks (List Task)
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTasks tasks ->
            ( { model | todo = tasks }, Cmd.none )

        Tick _ ->
            case model.current of
                Just task ->
                    let
                        nextTask =
                            { task | done = task.done + 1 }
                    in
                    if nextTask.done == nextTask.size then
                        ( { model | current = Nothing, done = { size = Hour <| toFloat nextTask.size } :: model.done }, Cmd.none )

                    else
                        ( { model | current = Just nextTask }, Cmd.none )

                Nothing ->
                    case model.todo of
                        first :: rest ->
                            ( { model | todo = rest, current = Just { size = Duration.toHours first.size |> round, done = 0 } }, Cmd.none )

                        [] ->
                            ( model, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 200 Tick



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Dev Team Sim"
    , body =
        [ Html.main_ []
            [ Html.section [ Html.Attributes.class "task-list" ] (Html.h2 [] [ Html.text "Todo" ] :: (model.todo |> List.map viewTask))
            , viewCharacter model
            , Html.section [ Html.Attributes.class "task-list" ] (Html.h2 [] [ Html.text "Done" ] :: (model.done |> List.map viewTask))
            , Html.section [ Html.Attributes.id "statistics" ]
                [ Html.h2 [] [ Html.text "Statistics" ]
                , viewMetric { label = "WIP", value = "1" }
                , viewMetric { label = "Throughput", value = Throughput.fromList model.done settings |> Throughput.print }
                , viewMetric { label = "Lead Time", value = "0.25 d/task" }
                , viewMetric { label = "Cost", value = "75 $/h" }
                ]
            ]
        ]
    }


viewMetric : { label : String, value : String } -> Html.Html Msg
viewMetric { label, value } =
    Html.div [ Html.Attributes.class "metric" ]
        [ Html.span [ Html.Attributes.class "label" ] [ Html.text label ]
        , Html.span [ Html.Attributes.class "value" ] [ Html.text value ]
        ]


viewTask : Task -> Html.Html Msg
viewTask task =
    Html.div [ Html.Attributes.class "task" ] [ Html.text <| Duration.print task.size ]


viewCharacter : Model -> Html.Html Msg
viewCharacter model =
    Html.section [ Html.Attributes.class "character" ]
        [ Html.img
            [ Html.Attributes.classList [ ( "working", model.current /= Nothing ) ]
            , Html.Attributes.src "[VITE_PLUGIN_ELM_ASSET:/static/img/characters/char_26.png]"
            ]
            []
        , case model.current of
            Just task ->
                Html.div [ Html.Attributes.class "work" ]
                    [ Html.span [ Html.Attributes.class "task" ] [ Html.text <| "Size: " ++ String.fromInt task.size ]
                    , Html.progress
                        [ Html.Attributes.value <| String.fromInt task.done
                        , Html.Attributes.max <| String.fromInt task.size
                        ]
                        []
                    ]

            Nothing ->
                Html.text ""
        ]
