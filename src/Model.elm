module Model exposing (Model)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Dict exposing (Dict)
import Key

type alias Model =
    { time   : Float
    , keys   : Dict String Key.Status
    , theta  : Float
    , pointer : Vec3
    }