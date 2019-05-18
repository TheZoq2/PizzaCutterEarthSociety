module Camera exposing (Camera, update, lookAtMatrix, cameraPos)

import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import Key exposing (keyStatusToInt)

type alias Camera =
    { lookingAt : Vec3
    , cameraOff : Vec3
    , dist : Float
    , up : Vec3
    }

bladeCoordInWorld : Mat4 -> Vec3 -> Vec3
bladeCoordInWorld bladeMat bladeCoord =
    Mat4.transform bladeMat bladeCoord

pan : Mat4 -> Vec3 -> Camera -> Camera
pan bladeMat change camera =
    let ch = bladeCoordInWorld bladeMat change
    in { camera | lookingAt = Vec3.add camera.lookingAt ch}


panGlobal : Vec3 -> Camera -> Camera
panGlobal change camera = { camera | lookingAt = Vec3.add camera.lookingAt change }

update : Mat4 -> Dict String Key.Status -> Camera -> Camera
update bladeMatrix dict camera =
    if Key.isDown dict "KeyA" then
        pan bladeMatrix (vec3 -1 0 0) camera
    else if Key.isDown dict "KeyD" then
        pan bladeMatrix (vec3 1 0 0) camera
    else if Key.isDown dict "KeyW" then
        pan bladeMatrix (vec3 0 1 0) camera
    else if Key.isDown dict "KeyS" then
        pan bladeMatrix (vec3 0 -1 0) camera
    else if Key.isDown dict "ArrowLeft" then
        panGlobal (vec3 -1 0 0) camera
    else if Key.isDown dict "ArrowRight" then
        panGlobal (vec3 1 0 0) camera
    else if Key.isDown dict "ArrowUp" then
        panGlobal (vec3 0 -1 0) camera
    else if Key.isDown dict "ArrowDown" then
        panGlobal (vec3 0 1 0) camera
    else if Key.isDown dict "Digit1" then
             { camera | dist = camera.dist * 0.9 }
    else if Key.isDown dict "Digit2" then
             { camera | dist = camera.dist * 1.1 }
    else let j = Key.isDown dict "KeyJ"
             p = Key.isDown dict "KeyP"
             c = Key.isDown dict "KeyC"
             v = Key.isDown dict "KeyV"
         in if j || p then
             let a = if j then -1 else 1
                 b = if p then 1 else -1
             in { camera | cameraOff =
                         Mat4.transform
                             (Mat4.makeRotate (0.1 * toFloat (a + b)) <| vec3 0 0 -1)
                             camera.cameraOff }
         else if c || v then
                     let a = if c then -1 else 1
                         b = if v then 1 else -1
                     in { camera | cameraOff =
                             Mat4.transform
                                 (Mat4.makeRotate (0.1 * toFloat (a + b)) <| vec3 -1 0 0)
                                 camera.cameraOff }
                 else camera


lookAtMatrix : Camera -> Mat4
lookAtMatrix camera =
    Mat4.makeLookAt
         (cameraPos camera)
         camera.lookingAt
         camera.up

cameraPos : Camera -> Vec3
cameraPos camera = Vec3.add camera.lookingAt <| Vec3.scale camera.dist camera.cameraOff
