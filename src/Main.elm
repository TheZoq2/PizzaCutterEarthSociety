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
import Platform.Cmd
import Set exposing (Set)
import Random
import Random.List exposing (choose)
import Time

import Meshes exposing (..)
import Unit exposing (Unit, newUnit)
import Selection exposing (intersections, CameraParameters)
import Model exposing (Model, Selected(..), UnitTool(..))
import Building exposing (buildingName, allBuildings, newBuilding)
import Msg exposing (Msg (..))
import Key
import Camera exposing (Camera, lookAtMatrix, cameraPos)
import Config
import Resource exposing (newResourceSite, resourceSiteMeshes)

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
    , buildings = Dict.empty
    , nextBuildingId = 0
    , camera = Camera (vec3 1 0 0) (vec3 0 0 0) 1
    , resourceSites = Dict.fromList [(0, newResourceSite Resource.Food (vec3 -0.5 0 0))]
    , nextResourceId = 1
    , resources =
        [ (Resource.Food, 10)
        , (Resource.Iron, 10)
        , (Resource.Wood, 10)
        , (Resource.Gold, 10)
        ]
    , nextRandom = 0
    }

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    let
        cmd =
            case message of
                SlowTick _ -> Random.generate NewRandom <| Random.pair (Random.float 0 1) (choose Resource.allResources)
                _ -> Cmd.none

        next_model =
            case message of
                Tick elapsed ->
                    updateResourceSites
                     <| updateBuildings (elapsed / 1000)
                     <| updateUnits (elapsed / 1000)
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
                BuildUnit building ->
                    case Dict.get building model.buildings of
                        Just {position} -> {model | units = model.units ++ [newUnit position]}
                        Nothing -> model
                NewRandom (val, (kind, _)) ->
                    if val < 0.2 then
                        let
                            pos =
                                worldCoordInBlade
                                    (bladeMatrix model.time)
                                    (vec3 (val / 0.2) 0 0)

                            kind_ = Maybe.withDefault Resource.Gold kind
                        in
                            { model
                                | resourceSites =
                                    Dict.insert
                                        model.nextResourceId
                                        (newResourceSite kind_ pos)
                                        model.resourceSites
                                , nextResourceId = model.nextResourceId + 1
                            }
                    else
                        model
                SlowTick _ -> model
    in (next_model, cmd)



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
            Just (SUnit units (Just (Build buildingKind))) ->
                tryBuildBuilding mousePos buildingKind units model
            Just (SBuilding _) ->
                trySelect mousePos model


cubeSelection : Model -> (Int, Int) -> Vec3 -> List (a, Vec3) -> List a
cubeSelection model mousePos size selectionOptions =
    List.filterMap (\(hit, a) -> if hit then Just a else Nothing)
        <| List.map
            (\(a, position) ->
                ( mouseIntersections
                    (Meshes.cubeTriangles size)
                    position
                    model
                    mousePos
                    |> List.isEmpty |> not
                , a
                )
            )
            selectionOptions


trySelect : (Int, Int) -> Model -> Model
trySelect mousePos model =
    let
        selectedUnits =
            cubeSelection
                model
                mousePos
                Config.unitSize
                <| List.indexedMap (\i {position} -> (i, position)) model.units

        selectedBuildings =
            cubeSelection
                model
                mousePos
                Config.buildingSize
                <| List.map (\(i, {position}) -> (i, position))
                <| Dict.toList (model.buildings)

        selectedUnit = Maybe.map (\id -> SUnit [id] Nothing) <| List.head selectedUnits
        selectedBuilding = Maybe.map (\id -> SBuilding id) <| List.head selectedBuildings

        finalSelection =
            case selectedUnit of
                Just s -> Just s
                Nothing -> selectedBuilding
    in
        { model | selected = finalSelection}

tryBuildBuilding : (Int, Int) -> Building.Kind -> List Int -> Model -> Model
tryBuildBuilding mousePos kind units model =
    let
        pos = List.head <| cutterMouseIntersections model mousePos
        id = model.nextBuildingId
    in
        case pos of
            Just position ->
                {model
                    | buildings =
                        Dict.insert id (newBuilding kind position) model.buildings
                    , nextBuildingId = model.nextBuildingId + 1
                    , selected = Nothing
                    , units =
                        commandSelectedUnits
                            (Unit.BuildBuilding id)
                            model.selected
                            model.units
                }
            Nothing ->
                model



commandSelectedUnits : Unit.Goal -> Maybe Selected -> List Unit -> List Unit
commandSelectedUnits goal selected units =
    case selected of
        Just (SUnit indices _) ->
            List.indexedMap
                (\i unit ->
                    if List.member i indices then
                        Unit.setGoal goal unit
                    else
                        unit
                )
                units
        _ -> units


priorityMaybe : List (Maybe a) -> Maybe a
priorityMaybe options =
    List.foldl (\option acc ->
        case acc of
            Just a -> Just a
            Nothing ->
                case option of
                    Just a -> Just a
                    Nothing -> Nothing
        ) Nothing options

onRightClick : Model -> Model
onRightClick model =
    let
        mousePos = Maybe.withDefault (0,0) model.mousePos

        moveGoal =
            Maybe.map Unit.MoveTo <| List.head <| cutterMouseIntersections model mousePos

        selectedResourceSite =
            Maybe.map (Unit.Gather)
                <| List.head
                    <| cubeSelection
                        model
                        mousePos
                        Config.buildingSize
                        <| List.map (\(i, {position}) -> (i, position))
                        <| Dict.toList (model.resourceSites)

        goal = priorityMaybe [selectedResourceSite, moveGoal]

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


shouldLive : Model -> Vec3 -> Bool
shouldLive model pos =
    let
        pos_ = (bladeCoordInWorld (bladeMatrix model.time) pos)
    in
    if Vec3.getX pos_ > 0 then
        True
    else
        False


updateUnits : Float -> Model -> Model
updateUnits elapsedTime model =
    let
        (units, changes) = List.unzip
            <| List.map
                (Unit.updateUnit elapsedTime model.buildings model.resourceSites)
                model.units
    in
        List.foldl
            Model.applyModelChange
            {model | units = units}
            <| List.filterMap identity changes


updateBuildings : Float -> Model -> Model
updateBuildings elapsedTime model =
    let
        buildingFn units index building =
            let
                -- Filter all buildings that 
                buildingUnits =
                    List.length
                     <| List.filter
                        (\{position, goal} ->
                            let
                                distance = Vec3.distance building.position position
                            in
                                (distance < Config.buildRadius)
                                &&
                                (goal == Unit.BuildBuilding index)
                        )
                        units

                status =
                    case building.status of
                        Building.Unbuilt progress ->
                            if progress > 1 then
                                Building.Done
                            else
                                Building.Unbuilt
                                    (progress + (elapsedTime / Config.buildTime) * toFloat buildingUnits)
                        a -> a
            in
                {building | status = status}

        newBuildings =
            Dict.filter (\_ {position} -> shouldLive model position)
                <| Dict.map
                    (buildingFn model.units)
                    model.buildings
    in
        {model | buildings = newBuildings}


updateResourceSites : Model -> Model
updateResourceSites model =
    let
        newResourceSite =
            Dict.filter (\_ {position} -> shouldLive model position) model.resourceSites
    in
        { model | resourceSites = newResourceSite }


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
        , Time.every 1000 SlowTick
        ]


textureFiles =
    [ ( "aluminium", "../textures/aluminium.jpg" )
    , ( "norway", "../textures/norway.png" )
    ]

textureTasks =
    textureFiles
        |> List.map (\(name, filename) ->
                         Task.attempt (TextureLoaded name) (Texture.load filename))
        |> Platform.Cmd.batch


main : Program D.Value Model Msg
main =
    Browser.element
        { init = \_ -> ( init, textureTasks )
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

        renderBillboardMesh : Mat4 -> Texture -> WebGL.Entity
        renderBillboardMesh modelMatrix texture =
            WebGL.entityWith
                settings
                billboardVertexShader
                billboardFragmentShader
                billboardMesh
                { modelViewProjection = Mat4.mul (perspective model.camera) modelMatrix
                , tex = texture
                , cameraPos = cameraPos model.camera
                }

        bladeRotation = bladeMatrix model.time

        renderedBlade =
            case Dict.get "aluminium" model.textures of
              Just texture ->
                  [ renderTexturedMesh pizzaCutterBladeMesh bladeRotation texture ]
              Nothing ->
                  []

        drawBuilding {position, kind, status} =
            let
                size = Config.buildingSize
                baseColor =
                    case kind of
                        Building.House -> vec3 0 1 0
                        Building.Depot -> vec3 0 0 1

                color = Vec3.scale (if status == Building.Done then 1 else 0.5) baseColor

                fullPosition =
                    case status of
                        Building.Unbuilt progress ->
                            Vec3.add position (vec3 0 0 ((1-progress) * (Vec3.getX size/2)))
                        _ ->
                            position
            in
                renderMesh
                    (cubeMesh size color)
                    (Mat4.mul bladeRotation (Mat4.makeTranslate fullPosition))

        drawResource {position, kind, depletion} =
            let
                allMeshes = resourceSiteMeshes kind

                amountToTake = ceiling <| (1-depletion) * (toFloat <| List.length allMeshes)

                meshes = List.take amountToTake allMeshes
            in
                List.map
                    (\mesh ->
                        renderMesh
                            mesh
                            (Mat4.mul bladeRotation (Mat4.makeTranslate position))
                    )
                    meshes

        discObjects =
            ( model.units
                |> List.map (\{position} -> Mat4.mul bladeRotation (Mat4.makeTranslate position))
                |> List.map (renderMesh (cubeMesh Config.unitSize (vec3 1 0 0)))
            )
            ++ (List.map drawBuilding <| Dict.values model.buildings)
            ++ (List.foldl (++) [] (List.map drawResource <| Dict.values model.resourceSites))

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
        <|  [ Html.div [style "float" "right"]
                [ Html.h3 [] [Html.text "Pizza Cutter Earth Society"]
                , instructions
                ]
            ]
            ++
            [ WebGL.toHtmlWith options
                [ width <| Tuple.first Config.viewportSize
                , height <| Tuple.second Config.viewportSize
                , style "display" "block"
                , style "background-color" "white"
                , onDown MouseDown
                , onContextMenu MouseDown
                ]
                ( renderedBlade ++
                  [ renderMesh pizzaCutterHandleMesh <| Mat4.makeTranslate3 0 1 0
                  ] ++
                  discObjects
                )
            ]
            ++
            [ resourceDisplay model
            , buildMenu model
            ]


instructions : Html Msg
instructions =
    Html.ul []
        [ Html.li [] [Html.text "The game is a standard RTS"]
        , Html.li [] [Html.text "Units are red"]
        , Html.li [] [Html.text "Houses are green, they make more units"]
        , Html.li [] [Html.text "Resources depots are blue, this is where units leave resources"]
        , Html.li [] [Html.text "Click to select"]
        , Html.li [] [Html.text "Right click to command"]
        , Html.li [] [Html.text "Use WASD to move, JP to rotate and CV to rotate"]
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
            Html.div []
                <| List.map
                    buildingButton
                    allBuildings
        Just (SBuilding id) ->
            case Maybe.map .kind <| Dict.get id model.buildings of
                Just Building.House ->
                    Html.div [] [Html.button [Html.Events.onClick (BuildUnit id)] [Html.text "Build unit"]]
                _ -> Html.div [] []
        Nothing -> Html.div [] []


resourceDisplay : Model -> Html Msg
resourceDisplay model =
    Html.div []
        <| List.map (\t -> Html.span [] [Html.text t])
        <| List.map
            (\(resource, amount) ->
                (Resource.symbol resource) ++ ": " ++ (String.fromInt amount) ++ " | "
            )
        model.resources


perspectiveMatrix : Mat4
perspectiveMatrix = Mat4.makePerspective 60 1 0.01 50

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

        const vec3 lightDir = vec3(1.0, -1.0, -1.0);
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

type alias BillboardUniforms =
    { modelViewProjection : Mat4
    , cameraPos : Vec3
    , tex: Texture
    }

type alias BillboardFragment =
    { vTexCoords : Vec2 }

billboardVertexShader : Shader BillboardVertex BillboardUniforms BillboardFragment
billboardVertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec2 texCoords;
        uniform mat4 modelViewProjection;
        uniform vec3 cameraPos;
        varying vec2 vTexCoords;

        void main () {
            vec3 toCamera = normalize(cameraPos - position);
            vec3 up = vec3(0.0, 1.0, 0.0);
            vec3 right = cross(toCamera, up);
            vec3 pos = right * position.x + cross(right, toCamera) * position.y;
            gl_Position = modelViewProjection * vec4(pos, 1.0);
            vTexCoords = texCoords;
        }

    |]


billboardFragmentShader : Shader {} BillboardUniforms BillboardFragment
billboardFragmentShader =
    [glsl|

        precision mediump float;
        varying vec2 vTexCoords;
        uniform sampler2D tex;

        void main () {
            gl_FragColor = texture2D(tex, vTexCoords);
        }

    |]
