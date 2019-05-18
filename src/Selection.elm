module Selection exposing (intersections, getClickPosition, CameraParameters)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)


type alias CameraParameters =
    { cameraPosition : Vec3
    , invertedViewMatrix : Mat4
    , perspective : Mat4
    , viewportSize : (Int, Int)
    }


intersections : (Int, Int) -> CameraParameters -> List (Vec3, Vec3, Vec3) -> List Vec3
intersections mousePos cameraParameters triangles =
    let
        {cameraPosition, invertedViewMatrix, perspective} =
            cameraParameters

        origin = cameraPosition

        destination =
            -- TODO: Check if these parameters are correct
            getClickPosition cameraParameters mousePos

        direction =
            Vec3.direction destination origin

        clickedLocations =
            triangleClickPosition origin direction triangles
    in
        clickedLocations


triangleClickPosition : Vec3 -> Vec3 -> List ( Vec3, Vec3, Vec3 ) -> List Vec3
triangleClickPosition origin destination list =
    let
        intersect =
            rayTriangleIntersect origin destination
    in
        List.filterMap
            (\triangle ->
                intersect triangle
            )
            list




rayTriangleIntersect : Vec3 -> Vec3 -> ( Vec3, Vec3, Vec3 ) -> Maybe Vec3
rayTriangleIntersect rayOrigin rayDirection ( triangle0, triangle1, triangle2 ) =
    let
        epsilon =
            0.000001

        edge1 =
            Vec3.sub triangle1 triangle0

        edge2 =
            Vec3.sub triangle2 triangle0

        pvec =
            Vec3.cross rayDirection edge2

        det =
            Vec3.dot edge1 pvec
    in
        if det < epsilon then
            Nothing
        else
            let
                tvec =
                    Vec3.sub rayOrigin triangle0

                u =
                    Vec3.dot tvec pvec
            in
                if u < 0 || u > det then
                    Nothing
                else
                    let
                        qvec =
                            Vec3.cross tvec edge1

                        v =
                            Vec3.dot rayDirection qvec
                    in
                        if v < 0 || u + v > det then
                            Nothing
                        else
                            let
                                t =
                                    (Vec3.dot edge2 qvec) / det

                                v0 =
                                    (Vec3.getX rayOrigin) + t * (Vec3.getX rayDirection)

                                v1 =
                                    (Vec3.getY rayOrigin) + t * (Vec3.getY rayDirection)

                                v2 =
                                    (Vec3.getZ rayOrigin) + t * (Vec3.getZ rayDirection)
                            in
                                Just (vec3 v0 v1 v2)





getClickPosition : CameraParameters -> (Int, Int) -> Vec3
getClickPosition cameraParameters pos =
    let
        x =
            toFloat <| Tuple.first pos

        y =
            toFloat <| Tuple.second pos


        viewportWidth = toFloat <| Tuple.first cameraParameters.viewportSize
        viewportHeight = toFloat <| Tuple.second cameraParameters.viewportSize

        normalizedPosition =
            ( (x * 2) / viewportWidth - 1, (1 - y / viewportHeight * 2) )

        _ = Debug.log "Normalized position" normalizedPosition

        {cameraPosition, invertedViewMatrix, perspective} = cameraParameters

        homogeneousClipCoordinates =
            Vec4.vec4
                (Tuple.first normalizedPosition)
                (Tuple.second normalizedPosition)
                -1
                1

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse perspective)

        vec4CameraCoordinates =
            mulVector invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4 (Vec4.getX vec4CameraCoordinates) (Vec4.getY vec4CameraCoordinates) -1 0

        vec4WorldCoordinates =
            mulVector invertedViewMatrix direction

        vec3WorldCoordinates =
            vec3 (Vec4.getX vec4WorldCoordinates) (Vec4.getY vec4WorldCoordinates) (Vec4.getZ vec4WorldCoordinates)

        normalizedVec3WorldCoordinates =
            Vec3.normalize vec3WorldCoordinates

        origin =
            cameraPosition

        scaledDirection =
            Vec3.scale 20 normalizedVec3WorldCoordinates

        destination =
            Vec3.add origin scaledDirection
    in
        destination


mulVector : Mat4 -> Vec4.Vec4 -> Vec4.Vec4
mulVector mat v =
    let
        rec =
            Mat4.toRecord mat

        r1 =
            Vec4.vec4 rec.m11 rec.m12 rec.m13 rec.m14

        r2 =
            Vec4.vec4 rec.m21 rec.m22 rec.m23 rec.m24

        r3 =
            Vec4.vec4 rec.m31 rec.m32 rec.m33 rec.m34

        r4 =
            Vec4.vec4 rec.m41 rec.m42 rec.m43 rec.m44
    in
        Vec4.vec4 (Vec4.dot r1 v) (Vec4.dot r2 v) (Vec4.dot r3 v) (Vec4.dot r4 v)
