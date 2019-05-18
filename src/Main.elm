module Main exposing (main)

{-
   Rotating triangle, that is a "Hello, World!" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyUp, onKeyDown, onMouseDown, onMouseMove)
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown)
import Html exposing (Html)
import Html.Events
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Mouse as Mouse
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture as Texture exposing (Texture)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Json.Decode as D exposing (Value)
import Dict
import Task
import Selection exposing (intersections, CameraParameters)
import Meshes exposing (..)

import Set exposing (Set)

import Meshes exposing (..)
import Unit exposing (Unit, newUnit)
import Selection exposing (intersections, CameraParameters)
import Model exposing (Model, Selected(..), UnitTool(..))
import Building exposing (buildingName, allBuildings, newBuilding)
import Msg exposing (Msg (..))
import Key
import Camera
import Camera exposing (Camera, lookAtMatrix, cameraPos)
import Config

init : Model
init =
    { time = 0
    , keys = Set.empty
    , textures = Dict.empty
    , intersections = []
    , mousePos = Nothing
    , units = [newUnit (vec3 0.5 0 0), newUnit (vec3 0 0.5 0)]
    , cursor = Nothing
    , selected = Nothing
    , camera = Camera (vec3 0 0 0) (vec3 0 0 0)
    , buildings = []
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
                TimeDelta d  -> { model | camera = Camera.update model.keys model.camera }
                KeyChange status str -> { model | keys =
                                             case status of
                                                 Key.Up -> Set.remove str model.keys
                                                 Key.Down -> Set.insert str model.keys }
                MouseDown event ->
                    case event.button of
                        Mouse.SecondButton -> onRightClick model
                        Mouse.MainButton -> onLeftClick model
                        _ -> model
                MouseMove x y ->
                    { model | mousePos = Just (x, y)}
                TextureLoaded file (Ok texture) ->
                    { model | textures = Dict.insert file texture model.textures }
                TextureLoaded file err ->
                    let _ = Debug.log ("Could not load texture" ++ file) err
                    in model
                OnBuildingButton kind ->
                    case model.selected of
                        Just (SUnit units _) ->
                            { model | selected = Just (SUnit units (Just (Build kind)))}
                        _ ->
                            Debug.log "Warning: building button clicked without unit" model
    in (next_model, Cmd.none)



onLeftClick : Model -> Model
onLeftClick model =
    let
        mousePos = Maybe.withDefault (0, 0) model.mousePos
    in
        case model.selected of
            Nothing ->
                trySelect mousePos model
            Just (SUnit _ Nothing) ->
                trySelect mousePos model
            Just (SUnit _ (Just (Build buildingKind))) ->
                tryBuildBuilding mousePos buildingKind model


trySelect : (Int, Int) -> Model -> Model
trySelect mousePos model =
    let
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
        { model | selected = Maybe.map (\id -> SUnit [id] Nothing) <| List.head selected }

tryBuildBuilding : (Int, Int) -> Building.Kind -> Model -> Model
tryBuildBuilding mousePos kind model =
    let
        pos = List.head <| cutterMouseIntersections model mousePos
    in
        case pos of
            Just position ->
                {model
                    | buildings = model.buildings ++ [newBuilding kind position]
                    , selected = Nothing
                }
            Nothing ->
                model




onRightClick : Model -> Model
onRightClick model =
    let
        goal =
            Maybe.andThen
                (\pos -> List.head <| cutterMouseIntersections model pos)
                model.mousePos

        selectedUnits = case model.selected of
            Just (SUnit indexes _) -> indexes
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
        cam = model.camera

        invertedViewMatrix = (Mat4.inverseOrthonormal <| lookAtMatrix cam)
        params = CameraParameters
            (cameraPos cam)
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

bladeCoordInWorld : Mat4 -> Vec3 -> Vec3
bladeCoordInWorld bladeMat bladeCoord =
    Mat4.transform bladeMat bladeCoord

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
        { init = \_ -> ( init, Task.attempt (TextureLoaded "aluminium") (Texture.load "../textures/aluminium.jpg") )
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

        renderMesh : Mesh ColoredVertex -> Mat4 -> WebGL.Entity
        renderMesh mesh modelMatrix =
            WebGL.entityWith
                settings
                vertexShader
                fragmentShader
                mesh
                { modelViewProjection = Mat4.mul (perspective model.camera) modelMatrix
                , modelMatrix = modelMatrix
                }

        renderTexturedMesh : Mesh TexturedVertex -> Mat4 -> Texture -> WebGL.Entity
        renderTexturedMesh mesh modelMatrix texture =
            WebGL.entityWith
                settings
                texturedVertexShader
                texturedFragmentShader
                mesh
                { modelViewProjection = Mat4.mul (perspective model.camera) modelMatrix
                , modelMatrix = modelMatrix
                , tex = texture
                }

        bladeRotation = bladeMatrix model.time


        renderedBlade =
            case Dict.get "aluminium" model.textures of
              Just texture ->
                  [ renderTexturedMesh pizzaCutterBladeMesh bladeRotation texture ]
              Nothing ->
                  []

        cursor = 
            case model.cursor of
                Just pos ->
                    [ renderMesh
                        (cubeMesh (vec3 0.1 0.1 0.1) (vec3 0 1 0))
                        (Mat4.mul bladeRotation (Mat4.makeTranslate pos))
                    ]
                Nothing -> []

        drawBuilding {position, kind} =
            let
                color =
                    case kind of
                        Building.Green -> vec3 0 1 0
                        Building.Blue -> vec3 0 0 1
            in
                renderMesh
                    (cubeMesh (vec3 0.1 0.1 0.1) color)
                    (Mat4.mul bladeRotation (Mat4.makeTranslate position))

        discObjects =
            cursor
            ++
            ( model.units
                |> List.map (\{position} -> Mat4.mul bladeRotation (Mat4.makeTranslate position))
                |> List.map (renderMesh (cubeMesh (vec3 0.05 0.05 0.2) (vec3 1 0 0)))
            )
            ++ (List.map drawBuilding model.buildings)

        onDown =
            { stopPropagation = True, preventDefault = True }
                |> Mouse.onWithOptions "mousedown"
        onContextMenu =
            { stopPropagation = True, preventDefault = True }
                |> Mouse.onWithOptions "contextmenu"
    in Html.div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        ]
        <| [ WebGL.toHtmlWith options
                [ width <| Tuple.first Config.viewportSize
                , height <| Tuple.second Config.viewportSize
                , style "display" "block"
                , style "background-color" "white"
                , onDown MouseDown
                , onContextMenu MouseDown
                ]
                ( renderedBlade ++
                  [ renderMesh pizzaCutterHandleMesh <| Mat4.makeTranslate3 0 1 0
                  , renderMesh (cubeMesh (vec3 0.1 0.1 0.1) (vec3 1 1 1)) <| Mat4.makeTranslate model.camera.base
                  , renderMesh (cubeMesh (vec3 0.1 0.1 0.1) (vec3 0 0 0)) <| Mat4.makeTranslate model.camera.lookingAt
                  ] ++
                  discObjects
                )
            ]
            ++
            [ buildMenu model
            ]


buildMenu : Model -> Html Msg
buildMenu model =
    let
        buildingButton building =
            Html.div
                []
                [Html.button
                    [Html.Events.onClick (OnBuildingButton building)]
                    [Html.text <| buildingName building]]
    in
    case model.selected of
        Just (SUnit _ _) ->
            -- Buildings
            let _ = Debug.log "processing selected" "" in
            Html.ul []
                <| List.map
                    buildingButton
                    allBuildings
        Nothing -> Html.div [] []

perspectiveMatrix : Mat4
perspectiveMatrix = Mat4.makePerspective 45 1 0.01 50

-- TODO: Rename to avoid conflicts with perspectiveMatrix, or rename perspectiveMatrix
-- to projection
perspective : Camera -> Mat4
perspective camera =
    Mat4.mul
        perspectiveMatrix
        (lookAtMatrix camera)


-- Shaders


type alias Uniforms a =
    { a | modelViewProjection : Mat4
    , modelMatrix : Mat4
    }

type alias FragmentData a =
    { a | worldPosition : Vec3 , worldNormal : Vec3 }

type alias ColoredFragement = FragmentData { vcolor : Vec3 }


vertexShader : Shader ColoredVertex (Uniforms a) ColoredFragement
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


fragmentShader : Shader {} (Uniforms a) ColoredFragement
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;
        varying vec3 worldPosition;
        varying vec3 worldNormal;

        const vec3 lightDir = vec3(1.0, -1.0, 1.0);
        const float ambientLight = 0.2;

        void main () {
            vec3 normal = normalize(worldNormal);
            float lightFactor = clamp(dot(normal, normalize(lightDir)), 0.0, 1.0);
            gl_FragColor = vec4((ambientLight + lightFactor) * vcolor, 1.0);
        }

    |]

type alias TexturedUniforms = Uniforms { tex : Texture }
type alias TexturedFragement = FragmentData { vTexCoords : Vec2 }

texturedVertexShader : Shader TexturedVertex TexturedUniforms TexturedFragement
texturedVertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 texCoords;
        uniform mat4 modelViewProjection;
        uniform mat4 modelMatrix;
        varying vec2 vTexCoords;
        varying vec3 worldPosition;
        varying vec3 worldNormal;

        void main () {
            gl_Position = modelViewProjection * vec4(position, 1.0);
            vTexCoords = texCoords;
            worldPosition = (modelMatrix * vec4(position, 1.0)).xyz;
            worldNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
        }

    |]


texturedFragmentShader : Shader {} TexturedUniforms TexturedFragement
texturedFragmentShader =
    [glsl|

        precision mediump float;
        varying vec2 vTexCoords;
        varying vec3 worldPosition;
        varying vec3 worldNormal;
        uniform sampler2D tex;

        const vec3 lightDir = vec3(1.0, -1.0, 1.0);
        const float ambientLight = 0.2;

        void main () {
            vec3 normal = normalize(worldNormal);
            float lightFactor = clamp(dot(normal, normalize(lightDir)), 0.0, 1.0);
            gl_FragColor = vec4(ambientLight + lightFactor * texture2D(tex, vTexCoords).rgb, 1);
        }

    |]
