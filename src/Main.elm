module Main exposing (main)

{-
   Rotating triangle, that is a "Hello, World!" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyUp, onKeyDown, onMouseDown, onMouseMove)

import Browser.Events exposing (onAnimationFrameDelta, onMouseDown)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Json.Decode as D exposing (Value)
import Selection exposing (intersections, CameraParameters)
import Html.Events.Extra.Mouse as Mouse
import Meshes exposing (..)
import Unit exposing (Unit, newUnit)
import Config

import Dict

import Model exposing (Model, Selected(..))
import Msg exposing (Msg (..))

import Key
import Camera

init : Model
init = { time = 0
       , keys = Dict.empty
       , theta = 3.0
       , intersections = []
       , mousePos = Nothing
       , units = [newUnit (vec3 0.5 0 0), newUnit (vec3 0 0.5 0)]
       , cursor = Nothing
       , selected = Nothing
       }


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    let
        next_model =
            case message of
                Tick elapsed ->
                    updateUnits
                        (elapsed / 1000)
                        { model
                            | time = model.time + elapsed
                            , cursor = Maybe.andThen
                                (\pos -> List.head <| cutterMouseIntersections model pos)
                                model.mousePos
                        }
                TimeDelta d  -> { model | theta = model.theta + Camera.posDelta model.keys d }
                KeyChange status str -> { model | keys = Key.update str status model.keys }
                MouseDown event ->
                    case event.button of
                        Mouse.MainButton -> onLeftClick model
                        Mouse.MiddleButton -> onRightClick model
                        _ -> model
                MouseMove x y ->
                    { model
                        | mousePos = Just (x, y)
                    }
    in (next_model, Cmd.none)



onLeftClick : Model -> Model
onLeftClick model =
    let
        mousePos = Maybe.withDefault (0, 0) model.mousePos
        selected =
            List.filterMap (\(hit, index) -> if hit then Just index else Nothing)
             <| List.indexedMap
                (\index {position} ->
                    ( mouseIntersections
                        (Meshes.cubeTriangles Config.unitSize)
                        position
                        model
                        mousePos
                        |> List.isEmpty |> not
                    , index
                    )
                )
                model.units
    in
        { model | selected = Maybe.map SUnit <| List.head selected }



onRightClick : Model -> Model
onRightClick model =
    let
        goal =
            Maybe.andThen
                (\pos -> List.head <| cutterMouseIntersections model pos)
                model.mousePos

        selectedUnits = case model.selected of
            Just (SUnit id) -> [id]
            _ -> []

        newUnits =
            case goal of
                Just goalUnwraped ->
                    List.indexedMap
                        (\index unit ->
                            if List.member index selectedUnits then
                                {unit | goal = goalUnwraped}
                            else
                                unit
                        )
                        model.units
                Nothing -> model.units
    in
        {model | units = newUnits}


updateUnits : Float -> Model -> Model
updateUnits elapsedTime model =
    {model | units = List.map (Unit.moveTowardsGoal elapsedTime) model.units}


cutterMouseIntersections : Model -> (Int, Int) -> List Vec3
cutterMouseIntersections model (x, y) =
    let
        triangles =
                pizzaCutterVertices
    in
        mouseIntersections triangles (vec3 0 0 0) model (x, y)


-- Checks for intersections with triangles on the board
mouseIntersections : List (Vec3, Vec3, Vec3) -> Vec3 -> Model -> (Int, Int) -> List Vec3
mouseIntersections triangles position model (x, y) =
    let
        t = model.theta

        invertedViewMatrix = (Mat4.inverseOrthonormal <| lookAtMatrix t)
        params = CameraParameters
            (cameraPos t)
            invertedViewMatrix
            perspectiveMatrix
            Config.viewportSize

        fullMatrix = Mat4.mul (bladeMatrix model.time) (Mat4.makeTranslate position)

        -- _ = Debug.log "fullMatrix" fullMatrix

        translated =
            List.map
                (\(a, b, c) ->
                    ( Mat4.transform fullMatrix a
                    , Mat4.transform fullMatrix b
                    , Mat4.transform fullMatrix c
                    )
                )
                triangles

        rayHits =
            intersections (x, y) params translated
            |> List.map (worldCoordInBlade fullMatrix)
    in
        rayHits


worldCoordInBlade : Mat4 -> Vec3 -> Vec3
worldCoordInBlade bladeMat worldCoord =
    Mat4.transform (Maybe.withDefault Mat4.identity <| Mat4.inverse bladeMat) worldCoord


mouseDecoder : (Int -> Int -> Msg) -> D.Decoder Msg
mouseDecoder msg =
    D.map2 msg (D.field "clientX" D.int) (D.field "clientY" D.int)

mouseDownDecoder : (Int -> Msg) -> D.Decoder Msg
mouseDownDecoder msg =
    D.map msg (D.field "button" D.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp   <| Key.decoder KeyChange Key.Up
        , onKeyDown <| Key.decoder KeyChange Key.Down
        , onAnimationFrameDelta TimeDelta
        , onMouseMove <| mouseDecoder MouseMove
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
bladeMatrix time = Mat4.makeRotate (time / Config.angularDivider / 1000) (vec3 0 0 1)


view : Model -> Html Msg
view model =
    let
        settings =
            [ Settings.cullFace Settings.back
            , DepthTest.default
            ]

        options =
            [ WebGL.clearColor 0.8 0.8 1.0 1.0
            , WebGL.depth 1
            , WebGL.antialias
            , WebGL.alpha True
            ]

        renderMesh : Mesh Vertex -> Mat4 -> WebGL.Entity
        renderMesh mesh modelMatrix =
            WebGL.entityWith
                settings
                vertexShader
                fragmentShader
                mesh
                { modelViewProjection = Mat4.mul (perspective model.theta) modelMatrix
                , modelMatrix = modelMatrix
                }

        bladeRotation = bladeMatrix model.time

        cursor = 
            case model.cursor of
                Just pos ->
                    [ renderMesh
                        (cubeMesh (vec3 0.1 0.1 0.1) (vec3 0 1 0))
                        (Mat4.mul bladeRotation (Mat4.makeTranslate pos))
                    ]
                Nothing -> []

        discObjects =
            cursor
            ++
            ( model.units
                |> List.map (\{position} -> Mat4.mul bladeRotation (Mat4.makeTranslate position))
                |> List.map (renderMesh (cubeMesh (vec3 0.05 0.05 0.2) (vec3 1 0 0)))
            )

        onDown =
            { stopPropagation = True, preventDefault = True }
                |> Mouse.onWithOptions "mousedown"
    in WebGL.toHtmlWith options
        [ width <| Tuple.first Config.viewportSize
        , height <| Tuple.second Config.viewportSize
        , style "display" "block"
        , style "background-color" "white"
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , onDown MouseDown
        ]
        ( [ renderMesh pizzaCutterBladeMesh bladeRotation
          , renderMesh pizzaCutterHandleMesh <| Mat4.makeTranslate3 0 1 0
          ] ++
          discObjects
        )

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
            float lightFactor = clamp(dot(normal, lightDir), 0.0, 1.0);
            gl_FragColor = ambientLight + lightFactor * vec4(vcolor, 1.0);
        }

    |]





