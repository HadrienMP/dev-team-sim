module Core.Settings exposing (..)

import Lib.Duration exposing (Duration(..))


type alias Settings =
    { dayLength : Duration
    , breakDuration : Duration
    , numberOfDevs : Int
    }


default : Settings
default =
    { dayLength = Hour 8
    , breakDuration = Hour 1
    , numberOfDevs = 1
    }
