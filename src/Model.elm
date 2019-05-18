module Model exposing (Model)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Dict exposing (Dict)
import Key
import Unit exposing (Unit)

type alias Model =
    { time   : Float
    , keys   : Dict String Key.Status
    , theta  : Float
    , intersections : List Vec3
    , mousePos : Maybe (Int, Int)
    , units : List Unit
    , cursor : Maybe Vec3
    }
