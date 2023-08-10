module Core.Stuff_Tests exposing (..)

import Expect
import Lib.Duration exposing (Duration(..))
import Test



-- TODO: avance d'une tâche sans pause


type TaskStatus
    = Done


suite : Test.Test
suite =
    Test.describe "Stuff"
        [ Test.test "fsdf" <|
            \_ ->
                advance
                    |> Expect.equal
                        { estimate = Hour 1, spent = Hour 1, status = Done }
        ]


advance : { estimate : Duration, spent : Duration, status : TaskStatus }
advance =
    { estimate = Hour 1, spent = Hour 1, status = Done }
