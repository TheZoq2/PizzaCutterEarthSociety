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

init : Model
init = { time = 0
       , keys = Dict.empty
       , theta = 3.0
       , pointer = (vec3 0 0 0)
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
                    let
                        _ = Debug.log "mousedown" (x, y)

                        t = model.theta

                        invertedViewMatrix = (Mat4.inverseOrthonormal <| lookAtMatrix t)
                        params = CameraParameters (cameraPos t) invertedViewMatrix perspectiveMatrix

                        (intersected, point) = intersections (x, y) params [triangle]

                        _ = Debug.log "Hit: " intersected
                    in
                        { model | pointer = point }
    in (next_model, Cmd.none)


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
        ]
        [ WebGL.entityWith
                settings
                vertexShader
                fragmentShader
                pizzaCutterBladeMesh
                { matrix = perspective model.theta, objectPosition = (vec3 0 0 0) }
        , WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            pizzaCutterHandleMesh
            { matrix = perspective model.theta, objectPosition = (vec3 0 0 0) }
        , WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            mesh
            { matrix = perspective model.theta, objectPosition = (vec3 0 0 0) }
        , WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            pointerMesh
            { matrix = perspective model.theta, objectPosition = (model.pointer) }
        ]

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
    , color : Vec3
    }


-- TODO: This needs to be synced with mesh at some point

triangle : (Vec3, Vec3, Vec3)
triangle =
    (vec3 0 0 0, vec3 1 0 1, vec3 1 -1 0)

mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 0 1) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]

pointerMesh : Mesh Vertex
pointerMesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 0.4 0 0.4) (vec3 0 1 0)
          , Vertex (vec3 0.4 -0.4 0) (vec3 0 0 1)
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
        |> List.map (\pos -> Vertex pos (vec3 0.5 0.5 0.5))
        |> WebGL.triangleFan


pizzaCutterHandleMesh : Mesh Vertex
pizzaCutterHandleMesh =
    let
        handlePositions y =
            [ vec3 -0.1 y -0.1 -- bottom 0, top 4
            , vec3 0.1 y -0.1  -- bottom 1, top 5
            , vec3 -0.1 y 0.1  -- bottom 2, top 6
            , vec3 0.1 y 0.1   -- bottom 3, top 7
            ]

        handleAttributes =
            handlePositions -0.2 ++ handlePositions 2
                |> List.map (\pos -> Vertex pos (vec3 0.4 0.4 0.3))

        handleIndices =
            [ ( 0, 1, 2 ) -- bottom
            , ( 1, 3, 2 )
            , ( 4, 5, 6 ) -- top
            , ( 5, 7, 6 )
            , ( 0, 1, 4 ) -- zmin side
            , ( 1, 4, 5 )
            , ( 2, 3, 6 ) -- zmax side
            , ( 3, 6, 7 )
            , ( 0, 2, 4 ) -- xmin side
            , ( 2, 4, 6 )
            , ( 1, 3, 5 ) -- xmax side
            , ( 3, 5, 7 )
            ]
    in
        WebGL.indexedTriangles handleAttributes handleIndices


-- Shaders


type alias Uniforms =
    { matrix : Mat4, objectPosition : Vec3}


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        uniform vec3 objectPosition;
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 matrix;
        varying vec3 vcolor;

        void main () {
            gl_Position = matrix * vec4(position + objectPosition, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]





