module Main exposing (..)

import Browser
import Dict exposing (update)
import Html exposing (label)
import Html.Attributes
import Platform exposing (Task)
import Platform.Cmd as Cmd
import Random exposing (Generator)
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


type alias Task =
    { size : Int
    , done : Int
    }


taskGenerator : Generator Task
taskGenerator =
    Random.uniform 2 [ 3, 5, 8, 13, 21 ]
        |> Random.andThen (\a -> Random.float 0.8 1.2 |> Random.map (\b -> toFloat a * b))
        |> Random.map round
        |> Random.map (\size -> { size = size, done = 0 })


type alias Model =
    { current : Maybe Task
    , todo : List Task
    , done : List Task
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { current = Nothing
      , todo = []
      , done = []
      }
    , taskGenerator |> Random.list 20 |> Random.generate GotTasks
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
                        ( { model | current = Nothing, done = nextTask :: model.done }, Cmd.none )

                    else
                        ( { model | current = Just nextTask }, Cmd.none )

                Nothing ->
                    case model.todo of
                        first :: rest ->
                            ( { model | todo = rest, current = Just first }, Cmd.none )

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
                , viewMetric { label = "Throughput", value = "4 points / j" }
                , viewMetric { label = "Lead Time", value = (leadTime model |> String.fromFloat) ++ " j" }
                , viewMetric { label = "Cost", value = "100$ / point" }
                , viewMetric { label = "1 Point", value = "~= 2h" }
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
    Html.div [ Html.Attributes.class "task" ] [ Html.text <| String.fromInt task.size ]


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


leadTime : Model -> Float
leadTime model =
    model.done
        |> List.map .size
        |> mean
        -- TODO: this level of pause is very extreme...
        -- 1 of pause between two tasks
        |> (+) 1
        -- 4 points are a day
        |> divfBy 4
        |> roundAt { place = 2 }


mean : List Int -> Float
mean list =
    list
        |> List.sum
        |> toFloat
        |> divfBy (List.length list |> toFloat)


roundAt : { place : Int } -> Float -> Float
roundAt { place } a =
    a * (10 ^ place |> toFloat) |> round |> toFloat |> divfBy (10 ^ place |> toFloat)


divfBy : Float -> Float -> Float
divfBy b a =
    a / b
