module Building exposing (..)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)

type Kind
    = Green
    | Blue


allBuildings = [Green, Blue]


buildingName : Kind -> String
buildingName building =
    case building of
        Green -> "Green"
        Blue -> "Blue"


type Status
    = Unbuilt Float
    | Done


type alias Building =
    { status : Status
    , kind : Kind
    , position : Vec3
    }

newBuilding : Kind -> Vec3 -> Building
newBuilding kind position =
    Building (Unbuilt 0) kind position
