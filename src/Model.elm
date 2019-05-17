module Model exposing (Model)

import Dict exposing (Dict)
import Key

type alias Model =
    { time   : Float
    , keys   : Dict String Key.Status
    , theta  : Float
    }
