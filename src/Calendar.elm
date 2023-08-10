module Calendar exposing (..)

import Core.Settings as Settings
import Lib.Duration exposing (Duration(..))


type alias Calendar =
    { day : Int
    , hour : Int
    }


init : Calendar
init =
    { day = 0, hour = 0 }


addHour : Calendar -> Calendar
addHour calendar =
    let
        next =
            { calendar | hour = calendar.hour + 1 }
    in
    if (Hour <| toFloat next.hour) == Settings.default.dayLength then
        { next | day = next.day + 1, hour = 0 }

    else
        next


print : Calendar -> String
print calendar =
    let
        month =
            calendar.day // 20

        monthStr =
            case month + 1 of
                1 ->
                    "Jan"

                2 ->
                    "Feb"

                3 ->
                    "Mar"

                4 ->
                    "Apr"

                5 ->
                    "May"

                6 ->
                    "Jun"

                7 ->
                    "Jul"

                8 ->
                    "Aug"

                9 ->
                    "Sep"

                10 ->
                    "Oct"

                11 ->
                    "Nov"

                12 ->
                    "Dec"

                _ ->
                    "Broken"

        dayLeft =
            calendar.day
                |> modBy 20
                |> (+) 1
                |> String.fromInt
    in
    monthStr ++ " " ++ dayLeft
