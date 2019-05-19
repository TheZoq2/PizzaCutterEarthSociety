module Meshes exposing (..)

import Math.Vector2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Array

type alias VertexBase a =
    { a | position : Vec3
    , normal : Vec3
    }

type alias ColoredVertex = VertexBase { color : Vec3 }
type alias TexturedVertex = VertexBase { texCoords : Vec2 }


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


circleTextureCoords : Vec3 -> Vec2
circleTextureCoords pos =
    vec2 (Vec3.getX pos / 2 + 0.5) (Vec3.getY pos / 2 + 0.5)

pizzaCutterVertices =
    circleVertices 1 16

pizzaCutterBladeMesh : Mesh TexturedVertex
pizzaCutterBladeMesh =
    pizzaCutterVertices
        |> List.map (\(pos1, pos2, pos3) ->
                ( { position = pos1, normal = (vec3 0 0 1), texCoords = circleTextureCoords pos1 }
                , { position = pos2, normal = (vec3 0 0 1), texCoords = circleTextureCoords pos2 }
                , { position = pos3, normal = (vec3 0 0 1), texCoords = circleTextureCoords pos3 }
                )
            )
        |> WebGL.triangles

type alias BillboardVertex = { position : Vec3, texCoords : Vec2 }

billboardMesh : Mesh BillboardVertex
billboardMesh =
    WebGL.triangles
        [ ( BillboardVertex (vec3 -0.5  0.5 0) (vec2 0 0)
          , BillboardVertex (vec3  0.5  0.5 0) (vec2 1 0)
          , BillboardVertex (vec3 -0.5 -0.5 0) (vec2 0 1)
          )
        , ( BillboardVertex (vec3 -0.5 -0.5 0) (vec2 0 1)
          , BillboardVertex (vec3  0.5  0.5 0) (vec2 1 0)
          , BillboardVertex (vec3  0.5 -0.5 0) (vec2 1 1)
          )
        ]

cubeVertices : Vec3 -> Vec3 -> List ColoredVertex
cubeVertices size color =
    let
        cornerX = Vec3.getX size / 2
        cornerY = Vec3.getY size / 2
        cornerZ = Vec3.getZ size / 2
        cubeVertex n p = { position = p, normal = n, color = color }
    in
    List.concat
        [ List.map (cubeVertex (vec3 0 -1 0))
              [ vec3 -cornerX -cornerY -cornerZ -- bottom
              , vec3  cornerX -cornerY -cornerZ
              , vec3 -cornerX -cornerY  cornerZ
              , vec3  cornerX -cornerY  cornerZ
              ]
        , List.map (cubeVertex (vec3 0 1 0))
              [ vec3 -cornerX cornerY -cornerZ -- cornerY
              , vec3 -cornerX cornerY  cornerZ
              , vec3  cornerX cornerY -cornerZ
              , vec3  cornerX cornerY  cornerZ
              ]
        , List.map (cubeVertex (vec3 0 0 -1))
              [ vec3 -cornerX -cornerY -cornerZ -- zmin side
              , vec3 -cornerX  cornerY -cornerZ
              , vec3  cornerX -cornerY -cornerZ
              , vec3  cornerX  cornerY -cornerZ
              ]
        , List.map (cubeVertex (vec3 0 0 1))
              [ vec3 -cornerX -cornerY cornerZ -- zmax side
              , vec3  cornerX -cornerY cornerZ
              , vec3 -cornerX  cornerY cornerZ
              , vec3  cornerX  cornerY cornerZ
              ]
        , List.map (cubeVertex (vec3 -1 0 0))
              [ vec3 -cornerX -cornerY  cornerZ -- xmin side
              , vec3 -cornerX  cornerY  cornerZ
              , vec3 -cornerX -cornerY -cornerZ
              , vec3 -cornerX  cornerY -cornerZ
              ]
        , List.map (cubeVertex (vec3 1 0 0))
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

cubeMesh : Vec3 -> Vec3 -> Mesh ColoredVertex
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



pizzaCutterHandleMesh : Mesh ColoredVertex
pizzaCutterHandleMesh =
    let
        handleColor =
            vec3 0.4 0.4 0.3

        handlePositions length = cubeVertices (vec3 0.1 length 0.1) handleColor

        handleAttributes =
            handlePositions 2.2
    in
        WebGL.indexedTriangles handleAttributes cubeIndices
