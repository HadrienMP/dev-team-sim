module Duration exposing (..)


type Duration
    = Hour Float


zero : Duration
zero =
    Hour 0


add : Duration -> Duration -> Duration
add (Hour a) (Hour b) =
    Hour (a + b)


multiply : Float -> Duration -> Duration
multiply multiplier (Hour a) =
    Hour (a * multiplier)


subtractBy : Duration -> Duration -> Duration
subtractBy (Hour b) (Hour a) =
    Hour (a - b)


divideBy : Duration -> Duration -> Float
divideBy (Hour b) (Hour a) =
    a / b


divideByInt : Int -> Duration -> Duration
divideByInt divider (Hour a) =
    a / toFloat divider |> Hour


toHours : Duration -> Float
toHours (Hour value) =
    value


isMultipleOf : Duration -> Duration -> Bool
isMultipleOf multiple target =
    (toHours target |> round) |> modBy (toHours multiple |> round) |> (==) 0


print : Duration -> String
print (Hour value) =
    let
        days =
            value / 8 |> floor

        daysStr =
            if days == 0 then
                ""

            else
                String.fromInt days ++ "d"

        hours =
            round value |> modBy 8

        hoursStr =
            if hours == 0 then
                ""

            else
                String.fromInt hours ++ "h"
    in
    daysStr ++ " " ++ hoursStr |> String.trim
