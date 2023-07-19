module Duration exposing (..)


type Duration
    = Hour Float


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


toHours : Duration -> Float
toHours (Hour value) =
    value
