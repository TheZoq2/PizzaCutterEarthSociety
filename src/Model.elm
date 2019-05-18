module Model exposing (Model, Selected(..))

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Dict exposing (Dict)
import WebGL.Texture exposing (Texture)

import Key
import Unit exposing (Unit)
import Set exposing (Set)

import Camera exposing (Camera)

type Selected
    = SUnit Int

type alias Model =
    { time   : Float
    , keys   : Set String
    , textures : Dict String Texture
    , camera : Camera
    , intersections : List Vec3
    , mousePos : Maybe (Int, Int)
    , units : List Unit
    , cursor : Maybe Vec3
    , selected : Maybe Selected
    }
