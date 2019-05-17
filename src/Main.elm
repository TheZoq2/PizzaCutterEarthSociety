module Triangle exposing (main)

{-
   Rotating triangle, that is a "Hello, World!" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Json.Decode exposing (Value)

type alias Model =
    { time : Float
    }

init : Model
init = {time = 0}


type Msg
    = Tick Float




update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        Tick elapsed -> ({ model | time = model.time + elapsed }, Cmd.none)





main : Program Value Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , subscriptions = (\_ -> onAnimationFrameDelta Tick)
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
            pizzaCutterMesh
            { perspective = perspective (model.time / 1000) }
        ]


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
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


pizzaCutterMesh : Mesh Vertex
pizzaCutterMesh =
    circleVertexPositions 1 32
        |> List.map (\pos -> Vertex pos (vec3 0.5 0.5 0.5))
        |> WebGL.triangleFan
-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
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
