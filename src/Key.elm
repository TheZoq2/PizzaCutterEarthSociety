module Key exposing (Status (..), decoder)

import Json.Decode as D exposing (Value)

type Status = Up | Down

decoder : (Status -> String -> msg) -> Status -> D.Decoder msg
decoder f status =
    D.map (f status)
          (D.field "code" D.string)
