module Msg exposing (Msg (..))

import Key
import WebGL.Texture as Texture exposing (Texture)

type Msg
    = Tick Float
    | KeyChange Key.Status String
    | TimeDelta Float
    | MouseDown Int Int
    | MouseMove Int Int
    | TextureLoaded (Result Texture.Error Texture)
