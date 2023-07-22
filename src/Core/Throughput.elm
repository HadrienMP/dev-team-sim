module Core.Throughput exposing (..)

import Core.DevTask exposing (Task)
import Core.Settings exposing (Settings)
import Extra exposing (roundAt)
import Lib.Duration as Duration exposing (Duration(..))


type Throughput
    = TasksPerDay Float


print : Throughput -> String
print (TasksPerDay value) =
    String.fromFloat value ++ "\u{00A0}tasks/d"


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


leadTime : { wip : Int, settings : Settings } -> Throughput -> Duration
leadTime { wip, settings } (TasksPerDay t) =
    settings.dayLength |> Duration.multiply (toFloat wip / t |> roundAt 2)
