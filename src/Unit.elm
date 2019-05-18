module Unit exposing (Unit, moveTowardsGoal)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import Config

type alias Unit =
    { position : Vec3
    , goal : Vec3
    }


moveTowardsGoal : Float -> Unit -> Unit
moveTowardsGoal elapsedTime unit =
    let
        moveAmount = elapsedTime * Config.unitSpeed

        directionLeft = Vec3.sub unit.position unit.goal
    in
        if Vec3.length directionLeft > moveAmount then
            { unit | position = Vec3.sub unit.position
                <| Vec3.scale moveAmount
                <| Vec3.direction unit.position unit.goal
            }
        else
            {unit | position = unit.goal}

