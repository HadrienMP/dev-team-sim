module DevTask exposing (..)

import Duration exposing (Duration(..))
import Random exposing (Generator)


type alias Task =
    { size : Duration }


random : Generator Task
random =
    Random.uniform 2 [ 3, 5, 8, 13, 21 ]
        |> Random.andThen addUncertainty
        |> Random.map (round >> toFloat >> Hour >> Task)


addUncertainty : Int -> Generator Float
addUncertainty a =
    Random.float 0.8 1.2 |> Random.map (\b -> toFloat a * b)
