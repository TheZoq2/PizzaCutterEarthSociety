module Building exposing (..)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)

type Kind
    = House
    | Depot


allBuildings = [House, Depot]


buildingName : Kind -> String
buildingName building =
    case building of
        House -> "House"
        Depot -> "Resource depot"


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



closestDepot: Vec3 -> List Building -> Maybe Vec3
closestDepot pos buildings =
    List.head
        <| List.map Tuple.second
        <| List.sortBy Tuple.first
        <| List.map (\{position} -> (Vec3.distance position pos, position))
        <| List.filter (\{kind} -> kind == Depot) buildings
