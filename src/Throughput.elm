module Throughput exposing (..)

import DevTask exposing (Task)
import Duration exposing (Duration(..))
import Extra exposing (roundAt)
import Settings exposing (Settings)


type Throughput
    = TasksPerDay Float


fromList : List Task -> Settings -> Throughput
fromList tasks settings =
    let
        nbTasks =
            List.length tasks |> toFloat

        taskDuration =
            tasks
                |> List.foldl (addTaskDuration settings) (Hour 0)
    in
    settings.dayLength
        |> Duration.divideBy taskDuration
        |> (*) nbTasks
        |> roundAt 2
        |> TasksPerDay


addTaskDuration : Settings -> Task -> Duration -> Duration
addTaskDuration settings task elapsedBefore =
    let
        elapsedAfter =
            task.size |> Duration.add elapsedBefore
    in
    elapsedAfter |> Duration.add (breakDuration elapsedAfter settings)


breakDuration : Duration -> Settings -> Duration
breakDuration elapsedAfter settings =
    if elapsedAfter |> Duration.isMultipleOf settings.dayLength then
        Hour 0

    else
        settings.breakDuration
