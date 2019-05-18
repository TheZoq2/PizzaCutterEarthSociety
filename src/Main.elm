module Main exposing (main)

{-
   Rotating triangle, that is a "Hello, World!" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyUp, onKeyDown, onMouseDown)

import Browser.Events exposing (onAnimationFrameDelta, onMouseDown)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as Settings
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Json.Decode as D exposing (Value)
import Selection exposing (intersections, CameraParameters)

import Dict

import Model exposing (Model)
import Msg exposing (Msg (..))

import Key
import Camera

viewportSize : (Int, Int)
viewportSize = (400, 400)

init : Model
init = { time = 0
       , keys = Dict.empty
       , theta = 3.0
       , intersections = []
       }


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    let
        next_model =
            case message of
                Tick elapsed -> { model | time  = model.time + elapsed }
                TimeDelta d  -> { model | theta = model.theta + Camera.posDelta model.keys d }
                KeyChange status str -> { model | keys = Key.update str status model.keys }
                MouseDown x y ->
                    { model | intersections = cutterMouseIntersections model (x, y) }
    in (next_model, Cmd.none)



cutterMouseIntersections : Model -> (Int, Int) -> List Vec3
cutterMouseIntersections model (x, y) =
    let
        t = model.theta

        invertedViewMatrix = (Mat4.inverseOrthonormal <| lookAtMatrix t)
        params = CameraParameters
            (cameraPos t)
            invertedViewMatrix
            perspectiveMatrix
            viewportSize

        bladeMat = (bladeMatrix model.time)
        triangles =
            List.map
                (\(a, b, c) ->
                    ( Mat4.transform bladeMat a
                    , Mat4.transform bladeMat b
                    , Mat4.transform bladeMat c
                    )
                )
                pizzaCutterVertices

        rayHits =
            intersections (x, y) params triangles
            |> List.map (worldCoordInBlade bladeMat)
    in
        rayHits

worldCoordInBlade : Mat4 -> Vec3 -> Vec3
worldCoordInBlade bladeMat worldCoord =
    Mat4.transform (Maybe.withDefault Mat4.identity <| Mat4.inverse bladeMat) worldCoord


mouseDecoder : D.Decoder Msg
mouseDecoder =
    D.map2 MouseDown (D.field "clientX" D.int) (D.field "clientY" D.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp   <| Key.decoder KeyChange Key.Up
        , onKeyDown <| Key.decoder KeyChange Key.Down
        , onAnimationFrameDelta TimeDelta
        , onMouseDown mouseDecoder
        ]


main : Program D.Value Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


bladeMatrix : Float -> Mat4
bladeMatrix time = Mat4.makeRotate (time / 1000) (vec3 0 0 1)

view : Model -> Html msg
view model =
    let
        settings = [Settings.cullFace Settings.back]
    in
    WebGL.toHtml
        [ width 400
        , height 400
        , style "display" "block"
        , style "background-color" "black"
        , style "position" "absolute"
        , style "top" "0"
        ,style "left" "0"
        ] <|
        [ WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            pizzaCutterBladeMesh
            ( let rotation = bladeMatrix model.time
              in { modelViewProjection = Mat4.mul (perspective model.theta) rotation
                 , modelMatrix = rotation
                 }
            )
        , WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            pizzaCutterHandleMesh
            { modelViewProjection = perspective model.theta
            , modelMatrix = Mat4.identity
            }
        ] ++
        List.map (\point ->
                ( WebGL.entityWith
                    settings
                    vertexShader
                    fragmentShader
                    pointerMesh
                    (let modelMatrix = Mat4.mul (bladeMatrix model.time) (Mat4.makeTranslate point)
                     in { modelViewProjection = Mat4.mul (perspective model.theta) modelMatrix
                        , modelMatrix = modelMatrix
                        })
                )
            )
            model.intersections

cameraPos : Float -> Vec3
cameraPos t = vec3 (4 * cos t) 0 (4 * sin t)

lookAtMatrix : Float -> Mat4
lookAtMatrix t =
    (Mat4.makeLookAt
         (cameraPos t) -- eye
         (vec3 0 0 0) -- center
         (vec3 0 1 0)) -- up

perspectiveMatrix : Mat4
perspectiveMatrix = Mat4.makePerspective 45 1 0.01 50

-- TODO: Rename to avoid conflicts with perspectiveMatrix, or rename perspectiveMatrix
-- to projection
perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (perspectiveMatrix)
        (lookAtMatrix (t))



-- Mesh


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


-- TODO: This needs to be synced with mesh at some point

triangle : (Vec3, Vec3, Vec3)
triangle =
    (vec3 0 0 0, vec3 1 1 0, vec3 1 -1 0)

mesh : Mesh Vertex
mesh =
    let normal = vec3 0 0 1
    in WebGL.triangles
        [ ( Vertex (vec3 0 0 0) normal (vec3 1 0 0)
          , Vertex (vec3 1 1 0) normal (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) normal (vec3 0 0 1)
          )
        ]

pointerMesh : Mesh Vertex
pointerMesh =
    let normal = vec3 0 0 1
    in WebGL.triangles
        [ ( Vertex (vec3 0 0 0) normal (vec3 1 0 0)
          , Vertex (vec3 0.4 0.4 0) normal (vec3 0 1 0)
          , Vertex (vec3 0.4 -0.4 0) normal (vec3 0 0 1)
          )
        ]

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
                ( Vertex pos1 (vec3 0 0 -1) (vec3 0.5 0.5 0.5)
                , Vertex pos2 (vec3 0 0 -1) (vec3 0.5 0.5 0.5)
                , Vertex pos3 (vec3 0 0 -1) (vec3 0.5 0.5 0.5)
                )
            )
        |> WebGL.triangles


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


type alias VertexToFragmentData =
    { vcolor : Vec3
    , worldPosition : Vec3
    , worldNormal : Vec3
    }


vertexShader : Shader Vertex Uniforms VertexToFragmentData
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        uniform mat4 modelViewProjection;
        uniform mat4 modelMatrix;
        varying vec3 vcolor;
        varying vec3 worldPosition;
        varying vec3 worldNormal;

        void main () {
            gl_Position = modelViewProjection * vec4(position, 1.0);
            vcolor = color;
            worldPosition = (modelMatrix * vec4(position, 1.0)).xyz;
            worldNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
        }

    |]


fragmentShader : Shader {} Uniforms VertexToFragmentData
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;
        varying vec3 worldPosition;
        varying vec3 worldNormal;

        const vec3 lightDir = vec3(1.0 / sqrt(3.0));
        const float ambientLight = 0.1;

        void main () {
            vec3 normal = normalize(worldNormal);
            float lightFactor = 1.;// dot(normal, lightDir);
            gl_FragColor = ambientLight + lightFactor * vec4(vcolor, 1.0);
        }

    |]





