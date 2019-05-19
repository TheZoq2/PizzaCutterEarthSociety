module Model exposing (Model, Selected(..), UnitTool(..), applyModelChange)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Dict exposing (Dict)
import WebGL.Texture exposing (Texture)
import Set exposing (Set)

import Key
import Unit exposing (Unit)
import Camera exposing (Camera)
import Building exposing (Building)
import Resource exposing (ResourceSite)
import ModelChange exposing (ModelChange)


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
    , resourceSites : Dict Int ResourceSite
    , nextResourceId : Int
    , resources : List (Resource.Kind, Int)
    }



applyModelChange : ModelChange -> Model -> Model
applyModelChange change model =
    let
        reduceResourceSite amount (index, site) =
            let _ = Debug.log "" site.depletion in
            if site.depletion < 1 then
                Just (index, { site | depletion = site.depletion + amount})
            else
                Nothing

        _ = Debug.log "Applying model change" change
    in
    case change of
        ModelChange.ReduceResourceStock index ->
            let
                newSites = Dict.fromList
                    <| List.filterMap (reduceResourceSite 0.05)
                    <| Dict.toList model.resourceSites
            in
                { model | resourceSites = newSites }
        ModelChange.AddResource index ->
            model
