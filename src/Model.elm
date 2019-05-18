module Model exposing (Model)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Dict exposing (Dict)

import Key
import Camera exposing (Camera)

type alias Model =
    { time   : Float
    , keys   : Dict String Key.Status
    , theta  : Float
    , camera : Camera
    , intersections : List Vec3
    , mousePos : Maybe (Int, Int)
    }
