module Unit exposing (Unit, updateUnit, newUnit, setGoal, Goal(..))

import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import Dict exposing (Dict)

import Config
import Building exposing (Building)
import Resource exposing (ResourceSite)
import ModelChange exposing (ModelChange(..))


type Goal
    = BuildBuilding Int
    | MoveTo Vec3
    | Gather Int
    | Deposit Int Resource.Kind

type alias Unit =
    { position : Vec3
    , goal : Goal
    }


newUnit : Vec3 -> Unit
newUnit pos =
    Unit pos (MoveTo pos)


updateUnit : Float -> Dict Int Building -> Dict Int ResourceSite -> Unit
   -> (Unit, Maybe ModelChange)
updateUnit elapsedTime buildings resources unit =
    let
        moveAmount = elapsedTime * Config.unitSpeed

        resourceKind siteIndex =
            Maybe.map (\{kind} -> kind)
                <| Dict.get siteIndex resources

        (goalPos, goalOnCompletion, onCompletion) =
            case unit.goal of
                BuildBuilding index ->
                    ( Maybe.withDefault unit.position
                        <| Maybe.map (\b -> b.position)
                        <| Dict.get index buildings
                    , BuildBuilding index
                    , Nothing
                    )
                -- When the unit is carrying resources
                Deposit index resource ->
                    ( Maybe.withDefault unit.position
                        <| Building.closestDepot
                           unit.position
                           (Dict.values buildings)
                    , Gather index
                    , Just <| AddResource resource
                    )
                -- When the unit has to fetch resources
                Gather index ->
                    ( Maybe.withDefault unit.position
                        <| Maybe.map (\b -> b.position)
                        <| Dict.get index resources
                    , Maybe.withDefault (MoveTo unit.position)
                        <| Maybe.map (Deposit index) (resourceKind index)
                    , Just <| ReduceResourceStock index
                    )
                MoveTo pos ->
                    (pos, MoveTo pos, Nothing)

        directionLeft = Vec3.sub unit.position goalPos
    in
        if Vec3.length directionLeft > moveAmount then
            ( { unit | position = Vec3.sub unit.position
                <| Vec3.scale moveAmount
                <| Vec3.direction unit.position goalPos
              }
            , Nothing
            )
        else
            ({unit | position = goalPos, goal = goalOnCompletion}, onCompletion)


setGoal : Goal -> Unit -> Unit
setGoal goal unit = {unit | goal = goal}
