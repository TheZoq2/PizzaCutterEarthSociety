module Camera exposing (Camera, update, lookAtMatrix, cameraPos)

import Set exposing (Set)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

type alias Camera =
    { base : Vec3
    , lookAt : Vec3
    }


type alias Input =
    { move : Vec3
    , lookMove : Vec3
    }

inputBase : Input
inputBase = { move = (vec3 0 0 0)
            , lookMove = (vec3 0 0 0) }

rotate : Vec3 -> Vec3
rotate v = vec3 (Vec3.getY v) -(Vec3.getX v) (Vec3.getZ v)

f : Vec3 -> String -> Input -> Input
f v s d =
    case s of
        "KeyW" -> { d | move = Vec3.negate v }
        "KeyA" -> { d | move = Vec3.negate <| rotate v }
        "KeyS" -> { d | move = v }
        "KeyD" -> { d | move = rotate v }

        "KeyV" -> { d | lookMove = Vec3.negate v }
        "KeyJ" -> { d | lookMove = Vec3.negate <| rotate v }
        "KeyC" -> { d | lookMove = v }
        "KeyP" -> { d | lookMove = rotate v }
        _ -> d


update : Set String -> Camera -> Camera
update set camera =
    let v   = Vec3.scale 0.1 (Vec3.direction camera.base camera.lookAt)
        inp = Set.foldl (f v) inputBase set
    in { camera
           | base = Vec3.add camera.base inp.move
           , lookAt = Vec3.add inp.move <| Vec3.add camera.lookAt inp.lookMove
       }




lookAtMatrix : Camera -> Mat4
lookAtMatrix camera =
    Mat4.makeLookAt
         (Vec3.add camera.base <| vec3 0 0 -2)
         camera.lookAt
         (vec3 0 0 -1)

cameraPos : Camera -> Vec3
cameraPos camera = Vec3.add camera.base (vec3 0 0 -1)
