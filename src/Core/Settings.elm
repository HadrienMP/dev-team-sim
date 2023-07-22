module Core.Settings exposing (..)

import Lib.Duration exposing (Duration)


type alias Settings =
    { dayLength : Duration
    , breakDuration : Duration
    }
