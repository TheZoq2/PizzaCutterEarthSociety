module Model exposing (Model)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Set exposing (Set)

import Camera exposing (Camera)

type alias Model =
    { time   : Float
    , keys   : Set String
    , camera : Camera
    , intersections : List Vec3
    , mousePos : Maybe (Int, Int)
    }
