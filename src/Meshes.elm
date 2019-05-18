module Meshes exposing (..)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Array

type alias Vertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


circleVertices : Float -> Int -> List (Vec3, Vec3, Vec3)
circleVertices radius numberOfSegments =
    List.map
        (\i -> let angle toAdd = turns (toFloat (i + toAdd) / toFloat numberOfSegments)
               in ( vec3 0 0 0
                  , vec3 (cos <| angle 1) (sin <| angle 1) 0
                  , vec3 (cos <| angle 0) (sin <| angle 0) 0
                  )
        )
        (List.range 0 numberOfSegments)


pizzaCutterVertices =
    circleVertices 1 16

pizzaCutterBladeMesh : Mesh Vertex
pizzaCutterBladeMesh =
    pizzaCutterVertices
        |> List.map (\(pos1, pos2, pos3) ->
                ( Vertex pos1 (vec3 0 0 1) (vec3 0.5 0.5 0.5)
                , Vertex pos2 (vec3 0 0 1) (vec3 0.5 0.5 0.5)
                , Vertex pos3 (vec3 0 0 1) (vec3 0.5 0.5 0.5)
                )
            )
        |> WebGL.triangles


cubeVertices : Vec3 -> Vec3 -> List Vertex
cubeVertices size color =
    let
        cornerX = Vec3.getX size / 2
        cornerY = Vec3.getY size / 2
        cornerZ = Vec3.getZ size / 2
    in
    List.concat
        [ List.map (\p -> Vertex p (vec3 0 -1 0) color)
              [ vec3 -cornerX -cornerY -cornerZ -- bottom
              , vec3  cornerX -cornerY -cornerZ
              , vec3 -cornerX -cornerY  cornerZ
              , vec3  cornerX -cornerY  cornerZ
              ]
        , List.map (\p -> Vertex p (vec3 0 1 0) color)
              [ vec3 -cornerX cornerY -cornerZ -- cornerY
              , vec3 -cornerX cornerY  cornerZ
              , vec3  cornerX cornerY -cornerZ
              , vec3  cornerX cornerY  cornerZ
              ]
        , List.map (\p -> Vertex p (vec3 0 0 -1) color)
              [ vec3 -cornerX -cornerY -cornerZ -- zmin side
              , vec3 -cornerX  cornerY -cornerZ
              , vec3  cornerX -cornerY -cornerZ
              , vec3  cornerX  cornerY -cornerZ
              ]
        , List.map (\p -> Vertex p (vec3 0 0 1) color)
              [ vec3 -cornerX -cornerY cornerZ -- zmax side
              , vec3  cornerX -cornerY cornerZ
              , vec3 -cornerX  cornerY cornerZ
              , vec3  cornerX  cornerY cornerZ
              ]
        , List.map (\p -> Vertex p (vec3 -1 0 0) color)
              [ vec3 -cornerX -cornerY  cornerZ -- xmin side
              , vec3 -cornerX  cornerY  cornerZ
              , vec3 -cornerX -cornerY -cornerZ
              , vec3 -cornerX  cornerY -cornerZ
              ]
        , List.map (\p -> Vertex p (vec3 1 0 0) color)
              [ vec3 cornerX -cornerY cornerZ -- xmax side
              , vec3 cornerX -cornerY -cornerZ
              , vec3 cornerX cornerY cornerZ
              , vec3 cornerX cornerY -cornerZ
              ]
        ]


cubeIndices : List (Int, Int, Int)
cubeIndices = 
    List.concatMap
        (\i -> [ ( i, i + 1, i + 2 ), ( i + 1, i + 3, i + 2 ) ])
        (List.map (\i -> i*4) <| List.range 0 5)

cubeMesh : Vec3 -> Vec3 -> Mesh Vertex
cubeMesh size color =
    WebGL.indexedTriangles (cubeVertices size color) cubeIndices


cubeTriangles : Vec3 -> List (Vec3, Vec3, Vec3)
cubeTriangles size =
    let
        array = Array.fromList
            <| List.map .position
            <| cubeVertices size (vec3 0 0 0)
    in
            List.map (\(a, b, c) ->
                    (Maybe.withDefault (vec3 0 0 0) <| Array.get a array
                    ,Maybe.withDefault (vec3 0 0 0) <| Array.get b array
                    ,Maybe.withDefault (vec3 0 0 0) <| Array.get c array
                    )
                )
                cubeIndices



pizzaCutterHandleMesh : Mesh Vertex
pizzaCutterHandleMesh =
    let
        handleColor =
            vec3 0.4 0.4 0.3

        handlePositions length = cubeVertices (vec3 0.1 length 0.1) handleColor

        handleAttributes =
            handlePositions 2.2
    in
        WebGL.indexedTriangles handleAttributes cubeIndices
