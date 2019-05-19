module Camera exposing (Camera, update, lookAtMatrix, cameraPos)

import Set exposing (Set)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import Config

type alias Camera =
    { base : Vec3
    , lookAt : Vec3
    , height : Float
    }


type alias Input =
    { move : Vec3
    , lookMove : Vec3
    , lookRotate : Float
    , height : Float
    }

inputBase : Input
inputBase = { move = (vec3 0 0 0)
            , lookMove = (vec3 0 0 0)
            , lookRotate = 0
            , height = 0 }

rotate : Vec3 -> Vec3
rotate v = vec3 (Vec3.getY v) -(Vec3.getX v) (Vec3.getZ v)

f : Vec3 -> String -> Input -> Input
f forward keyname d =
    case keyname of
        "KeyS" -> { d | move = Vec3.negate forward }
        "KeyD" -> { d | move = Vec3.negate <| rotate forward }
        "KeyW" -> { d | move = forward }
        "KeyA" -> { d | move = rotate forward }

        "KeyC" -> { d | lookMove = Vec3.negate forward }
        "KeyV" -> { d | lookMove = forward }
        "KeyJ" -> { d | lookRotate = 0.1 }
        "KeyP" -> { d | lookRotate = -0.1}

        "Digit1" -> { d | height =  0.1 }
        "Digit2" -> { d | height = -0.1 }
        _ -> d



update : Set String -> Camera -> Camera
update set camera =
    let v   = Vec3.sub camera.lookAt camera.base
        inp = Set.foldl (f <| Vec3.scale Config.cameraPanSpeed <| Vec3.normalize v) inputBase set
        newheight = camera.height + inp.height
        new_v = Vec3.sub v <| Vec3.scale (newheight / camera.height) v
    in { camera
           | base = Vec3.add camera.base <| Vec3.add inp.move new_v
           , lookAt = Vec3.add camera.base
                      <| Mat4.transform (Mat4.makeRotate inp.lookRotate <| vec3 0 0 -1)
                      <| Vec3.add inp.lookMove
                      <| Vec3.add inp.move v
           , height = newheight
       }





lookAtMatrix : Camera -> Mat4
lookAtMatrix camera =
    Mat4.makeLookAt
        (cameraPos camera)
        camera.lookAt
        (vec3 0 1 0)

cameraPos : Camera -> Vec3
cameraPos camera = Vec3.add camera.base <| Vec3.scale camera.height <| Vec3.negate Vec3.k
