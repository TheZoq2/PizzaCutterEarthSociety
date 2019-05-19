module Config exposing (..)

import Math.Vector3 as Vec3 exposing (vec3)

unitSpeed = 0.2
unitSize = vec3 0.025 0.025 0.1
buildingSize = vec3 0.05 0.05 0.05
buildRadius = 0.1
buildTime : Float
buildTime = 5.0
resourcesPerTrip = 0.1

angularDivider = 50

viewportSize : (Int, Int)
viewportSize = (600, 600)


cameraPanSpeed = 0.05
