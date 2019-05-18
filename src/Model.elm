module Model exposing (Model)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Dict exposing (Dict)
import Key
import WebGL.Texture exposing (Texture)

type alias Model =
    { time   : Float
    , keys   : Dict String Key.Status
    , textures : Dict String Texture
    , theta  : Float
    , intersections : List Vec3
    , mousePos : Maybe (Int, Int)
    }
