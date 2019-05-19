module Model exposing (Model, Selected(..), UnitTool(..))

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Dict exposing (Dict)
import WebGL.Texture exposing (Texture)
import Set exposing (Set)

import Key
import Unit exposing (Unit)
import Camera exposing (Camera)
import Building exposing (Building)


type UnitTool
    = Build Building.Kind

type Selected
    = SUnit (List Int) (Maybe UnitTool)
    | SBuilding Int

type alias Model =
    { time   : Float
    , keys   : Set String
    , textures : Dict String Texture
    , camera  : Camera
    , intersections : List Vec3
    , mousePos : Maybe (Int, Int)
    , units : List Unit
    , cursor : Maybe Vec3
    , selected : Maybe Selected
    , buildings : Dict Int Building
    , nextBuildingId : Int
    }
