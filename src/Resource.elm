module Resource exposing (Kind(..), ResourceSite, newResourceSite, resourceSiteMeshes, symbol, allResources)

import Meshes exposing (..)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh)




type Kind
    = Food
    | Wood
    | Iron
    | Gold


allResources : List Kind
allResources = [Food, Wood, Iron, Gold]


symbol : Kind -> String
symbol kind =
    case kind of
        Food -> "🍎"
        Wood -> "🌲"
        Iron -> "⛏"
        Gold -> "💎"



type alias ResourceSite =
    { kind : Kind
    , position : Vec3
    , depletion : Float
    }


newResourceSite : Kind -> Vec3 -> ResourceSite
newResourceSite kind position =
    ResourceSite kind position 0



resourceColors : Kind -> (Vec3, Vec3)
resourceColors kind =
    case kind of
        Food -> (vec3 0 0.6 0.1, vec3 0.8 0.2 0.17)
        Wood -> (vec3 0.25 0.77 0.15, vec3 0.14 0.39 0.15)
        Iron -> (vec3 0.39 0.39 0.39, vec3 0.64 0.45 0.18)
        Gold -> (vec3 0.13 0.62 0.87, vec3 0.12 0.28 0.87)



resourceSiteMeshes : Kind -> List (Mesh ColoredVertex)
resourceSiteMeshes kind =
    let
        scale = 0.05
        offsets =
            [ vec3 0 1 0.06
            , vec3 0.2 0 -0.05
            , vec3 0.4 0.3 0
            , vec3 -0.4 0.2 -0.02
            , vec3 -1 -0.2 0
            , vec3 -0.8 -0.6 0.03
            ] |> List.map (Vec3.scale scale)

        colorFromIndex i =
            (if (modBy 2 i) == 0 then Tuple.first else Tuple.second) (resourceColors kind)


        moveVertex offset vertex =
            {vertex | position = Vec3.add vertex.position offset}

        size = (vec3 0.03 0.03 0.03)

        vertices = List.indexedMap
            (\i offset -> List.map (moveVertex offset) <| cubeVertices size (colorFromIndex i))
            offsets
    in
            List.map (\vertex -> WebGL.indexedTriangles vertex cubeIndices) vertices
