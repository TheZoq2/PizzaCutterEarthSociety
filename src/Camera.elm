module Camera exposing (posDelta)

import Dict exposing (Dict)
import Maybe exposing (withDefault)

import Key exposing (keyStatusToInt)

posDelta : Dict String Key.Status -> Float -> Float
posDelta dict dt =
    let a = -(keyStatusToInt (withDefault Key.Up <| Dict.get "KeyJ" dict))
        b =  keyStatusToInt (withDefault Key.Up <| Dict.get "KeyP" dict)
    in dt * 0.01 * toFloat (a + b)
