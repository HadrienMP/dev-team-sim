module Throughput exposing (..)

import DevTask exposing (Task)
import Duration
import Extra exposing (roundAt)
import Settings exposing (Settings)



-- TODO: this assumes that you take a break first thing in the morning if your task spanned the whole day


type Throughput
    = TasksPerDay Float


fromList : List Task -> Settings -> Throughput
fromList tasks settings =
    let
        nbTasks =
            List.length tasks |> toFloat

        taskDuration =
            tasks
                |> List.map .size
                |> List.foldl Duration.add (Duration.multiply nbTasks settings.breakDuration)
    in
    settings.dayLength
        |> Duration.divideBy taskDuration
        |> (*) nbTasks
        |> roundAt 2
        |> TasksPerDay
