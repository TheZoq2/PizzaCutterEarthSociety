module Unit exposing (Unit, moveTowardsGoal, newUnit, setGoal, Goal(..))

import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import Dict exposing (Dict)

import Config
import Building exposing (Building)


type Goal
    = BuildBuilding Int
    | MoveTo Vec3

type alias Unit =
    { position : Vec3
    , goal : Goal
    }


newUnit : Vec3 -> Unit
newUnit pos =
    Unit pos (MoveTo pos)


moveTowardsGoal : Float -> Dict Int Building -> Unit -> Unit
moveTowardsGoal elapsedTime buildings unit =
    let
        moveAmount = elapsedTime * Config.unitSpeed

        goalPos =
            case unit.goal of
                BuildBuilding index ->
                    Maybe.withDefault unit.position
                        <| Maybe.map (\b -> b.position)
                        <| Dict.get index buildings
                MoveTo pos ->
                    pos

        directionLeft = Vec3.sub unit.position goalPos
    in
        if Vec3.length directionLeft > moveAmount then
            { unit | position = Vec3.sub unit.position
                <| Vec3.scale moveAmount
                <| Vec3.direction unit.position goalPos
            }
        else
            {unit | position = goalPos}


setGoal : Goal -> Unit -> Unit
setGoal goal unit = {unit | goal = goal}
