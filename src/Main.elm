module Main exposing (main)

{-
   Rotating triangle, that is a "Hello, World!" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyUp, onKeyDown)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Json.Decode as D

import Dict

import Model exposing (Model)
import Msg exposing (Msg (..))

import Key
import Camera

init : Model
init = { time = 0
       , keys = Dict.empty
       , theta = 3.0
       }


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    let next_model =
            case message of
                Tick elapsed -> { model | time  = model.time + elapsed }
                TimeDelta d  -> { model | theta = model.theta + Camera.posDelta model.keys d }
                KeyChange status str -> { model | keys = Key.update str status model.keys }
    in (next_model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp   <| Key.decoder KeyChange Key.Up
        , onKeyDown <| Key.decoder KeyChange Key.Down
        , onAnimationFrameDelta TimeDelta
        ]

main : Program D.Value Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width 400
        , height 400
        , style "display" "block"
        ]
    [ WebGL.entity
            vertexShader
            fragmentShader
            pizzaCutterBladeMesh
            { modelViewProjection = perspective model
            , modelMatrix = Mat4.identity
            }
    , WebGL.entity
        vertexShader
        fragmentShader
        pizzaCutterHandleMesh
        { modelViewProjection = perspective model
        , modelMatrix = Mat4.identity
        }
    ]

perspective : Model -> Mat4
perspective m =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt
             (vec3 (4 * cos m.theta) 0 (4 * sin m.theta)) -- eye
             (vec3 0 0 0) -- center
             (vec3 0 1 0)) -- up



-- Mesh


type alias Vertex =
    { position : Vec3
    , normals : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    let normal = vec3 0 0 1
    in WebGL.triangles
        [ ( Vertex (vec3 0 0 0) normal (vec3 1 0 0)
          , Vertex (vec3 1 1 0) normal (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) normal (vec3 0 0 1)
          )
        ]


circleVertexPositions : Float -> Int -> List Vec3
circleVertexPositions radius numberOfSegments =
    [ vec3 0 0 0
    ] ++
    List.map
        (\i -> let angle = turns (toFloat i / toFloat numberOfSegments)
               in vec3 (cos angle) (sin angle) 0)
        (List.range 0 numberOfSegments)


pizzaCutterBladeMesh : Mesh Vertex
pizzaCutterBladeMesh =
    circleVertexPositions 1 32
        |> List.map (\pos -> Vertex pos (vec3 0 0 -1) (vec3 0.5 0.5 0.5))
        |> WebGL.triangleFan


pizzaCutterHandleMesh : Mesh Vertex
pizzaCutterHandleMesh =
    let
        handleColor =
            vec3 0.4 0.4 0.3

        handlePositions bottom top = List.concat
            [ List.map (\p -> Vertex p (vec3 0 -1 0) handleColor)
                  [ vec3 -0.1 bottom -0.1 -- bottom
                  , vec3 0.1 bottom -0.1
                  , vec3 -0.1 bottom 0.1
                  , vec3 0.1 bottom 0.1
                  ]
            , List.map (\p -> Vertex p (vec3 0 1 0) handleColor)
                  [ vec3 -0.1 top -0.1 -- top
                  , vec3 0.1 top -0.1
                  , vec3 -0.1 top 0.1
                  , vec3 0.1 top 0.1
                  ]
            , List.map (\p -> Vertex p (vec3 0 0 -1) handleColor)
                  [ vec3 -0.1 bottom -0.1 -- zmin side
                  , vec3 0.1 bottom -0.1
                  , vec3 -0.1 top -0.1
                  , vec3 0.1 top -0.1
                  ]
            , List.map (\p -> Vertex p (vec3 0 0 1) handleColor)
                  [ vec3 -0.1 bottom -0.1 -- zmax side
                  , vec3 0.1 bottom -0.1
                  , vec3 -0.1 top -0.1
                  , vec3 0.1 top -0.1
                  ]
            , List.map (\p -> Vertex p (vec3 -1 0 0) handleColor)
                  [ vec3 -0.1 bottom 0.1 -- xmin side
                  , vec3 -0.1 bottom -0.1
                  , vec3 -0.1 top 0.1
                  , vec3 -0.1 top -0.1
                  ]
            , List.map (\p -> Vertex p (vec3 1 0 0) handleColor)
                  [ vec3 0.1 bottom 0.1 -- xmax side
                  , vec3 0.1 bottom -0.1
                  , vec3 0.1 top 0.1
                  , vec3 0.1 top -0.1
                  ]
            ]

        handleAttributes =
            handlePositions -0.2 2

        handleIndices =
            List.concatMap
                (\i -> [ ( i, i + 1, i + 2 ), ( i + 1, i + 3, i + 2 ) ])
                (List.map (\i -> i*4) <| List.range 0 5)
    in
        WebGL.indexedTriangles handleAttributes handleIndices


-- Shaders


type alias Uniforms =
    { modelViewProjection : Mat4
    , modelMatrix : Mat4
    }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vWorldPosition : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 modelViewProjection;
        uniform mat4 modelMatrix;
        varying vec3 vcolor;
        varying vec3 vWorldPosition;

        void main () {
            gl_Position = modelViewProjection * vec4(position, 1.0);
            vcolor = color;
            vWorldPosition = (modelMatrix * vec4(position, 1.0)).xyz;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, vWorldPosition : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;
        varying vec3 vWorldPosition;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
