module Camera exposing (Camera, update, lookAtMatrix, cameraPos)

import Set exposing (Set)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

type alias Camera =
    { base : Vec3
    , lookingAt : Vec3
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

type alias Input =
    { move : Vec3
    , lookMove : Vec3
    }

inputBase : Input
inputBase = { move = (vec3 0 0 0)
            , lookMove = (vec3 0 0 0) }

f : String -> Input -> Input
f s d = case s of
            "KeyA" -> { d | move = (vec3  1 0 0) }
            "KeyD" -> { d | move = (vec3 -1 0 0) }
            "KeyW" -> { d | move = (vec3 0  1 0) }
            "KeyS" -> { d | move = (vec3 0 -1 0) }
            "KeyJ" -> { d | lookMove = (vec3  1 0 0) }
            "KeyP" -> { d | lookMove = (vec3 -1 0 0) }
            "KeyC" -> { d | lookMove = (vec3 0  1 0) }
            "KeyV" -> { d | lookMove = (vec3 0 -1 0) }
            _ -> d

update : Set String -> Camera -> Camera
update set camera =
    let inp = Set.foldl f inputBase set
    in { camera
           | base = Vec3.add camera.base inp.move
           , lookingAt = Vec3.add camera.lookingAt inp.lookMove
       }


lookAtMatrix : Camera -> Mat4
lookAtMatrix camera =
    Mat4.makeLookAt
         (Vec3.add camera.base <| vec3 0 0 -1)
         camera.lookingAt
         (vec3 0 0 -1)

cameraPos : Camera -> Vec3
cameraPos camera = Vec3.add camera.base (vec3 0 0 -1)
