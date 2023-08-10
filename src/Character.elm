module Character exposing (..)

import Html exposing (h1)
import Html.Attributes
import Lib.Duration exposing (Duration(..))
import Random
import UUID exposing (UUID)


type BillingRate
    = EurosPerHour Int


type Id
    = Id UUID


type alias Character =
    { task : Maybe { done : Int, size : Int }
    , image : String
    , price : BillingRate
    , id : Id
    }


random : Random.Generator Character
random =
    Random.pair
        (UUID.generator |> Random.map Id)
        (Random.int 1 50)
        |> Random.map
            (\( id, imageIndex ) ->
                { task = Nothing
                , image =
                    "/public/img/characters/char_"
                        ++ String.padLeft 2 '0' (String.fromInt imageIndex)
                        ++ ".png"
                , price = EurosPerHour 75
                , id = id
                }
            )


progress : Character -> Character
progress character =
    case character.task of
        Just task ->
            { character | task = Just <| { task | done = min (task.done + 1) task.size } }

        Nothing ->
            character


view : Character -> Html.Html msg
view model =
    Html.section [ Html.Attributes.class "character" ]
        [ Html.img
            [ Html.Attributes.classList [ ( "working", model.task /= Nothing ) ]
            , Html.Attributes.src model.image
            ]
            []
        , case model.task of
            Just task ->
                Html.div [ Html.Attributes.class "work" ]
                    [ Html.span
                        [ Html.Attributes.class "task" ]
                        [ Html.text <| "Size: " ++ String.fromInt task.size ]
                    , Html.progress
                        [ Html.Attributes.value <| String.fromInt task.done
                        , Html.Attributes.max <| String.fromInt task.size
                        ]
                        []
                    ]

            Nothing ->
                Html.text ""
        ]
