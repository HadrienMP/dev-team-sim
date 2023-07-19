module ThroughputTests exposing (..)

import DevTask exposing (Task)
import Duration exposing (Duration(..))
import Expect
import Settings exposing (Settings)
import Test exposing (Test, describe, test)
import Throughput exposing (Throughput(..))


settings : Settings
settings =
    { dayLength = Hour 4
    , breakDuration = Hour 1
    }


suite : Test
suite =
    describe "Throughput"
        [ describe "Single task"
            [ describe "is the task size for a single task that is smaller than a day"
                [ test "3" <|
                    \_ ->
                        throughput [ { size = Hour 3 } ]
                            |> Expect.equal (TasksPerDay 1)
                , test "4" <|
                    \_ ->
                        throughput [ { size = Hour 4 } ]
                            |> Expect.equal (TasksPerDay 0.8)
                ]
            , describe "is the mean points done in one day"
                ([ { size = 8, expected = 0.44 }
                 , { size = 7, expected = 0.5 }
                 , { size = 5, expected = 0.67 }
                 ]
                    |> List.map
                        (\{ size, expected } ->
                            test (String.fromFloat size ++ "hours -> " ++ String.fromFloat expected ++ "tasks/day") <|
                                \_ ->
                                    [ { size = Hour size } ]
                                        |> throughput
                                        |> Expect.equal (TasksPerDay expected)
                        )
                )
            ]
        , describe "Multiple tasks"
            [ test "toto" <|
                \_ ->
                    [ { size = Hour 1 }, { size = Hour 1 } ]
                        |> throughput
                        |> Expect.equal (TasksPerDay 2)
            , test "toto2" <|
                \_ ->
                    [ { size = Hour 3 }, { size = Hour 3 } ]
                        |> throughput
                        |> Expect.equal (TasksPerDay 1)
            ]
        ]


throughput : List Task -> Throughput.Throughput
throughput tasks =
    Throughput.fromList tasks settings
