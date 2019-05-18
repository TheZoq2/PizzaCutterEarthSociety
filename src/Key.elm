module Key exposing (Status (..), keyStatusToInt, update, decoder, isDown)

import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Json.Decode as D exposing (Value)

type Status = Up | Down

keyStatusToInt : Status -> Int
keyStatusToInt st =
    case st of
        Up -> -1
        Down -> 1


update : String -> Status -> Dict String Status -> Dict String Status
update = Dict.insert


decoder : (Status -> String -> msg) -> Status -> D.Decoder msg
decoder f status =
    D.map (f status)
          (D.field "code" D.string)


isDown : Dict String Status -> String -> Bool
isDown dict name =
    (withDefault Up <| Dict.get name dict) == Down
