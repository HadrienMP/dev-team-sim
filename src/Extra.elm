module Extra exposing (..)


roundAt : Int -> Float -> Float
roundAt place toRound =
    let
        stuff =
            10 ^ place |> toFloat
    in
    toRound
        * stuff
        |> round
        |> (\a -> toFloat a / stuff)
