module Msg exposing (Msg (..))

import WebGL.Texture as Texture exposing (Texture)
import Html.Events.Extra.Mouse as Mouse
import Time

import Key
import Building
import Resource

type Msg
    = Tick Float
    | KeyChange Key.Status String
    | TimeDelta Float
    | MouseDown Mouse.Event
    | MouseMove Int Int
    | TextureLoaded String (Result Texture.Error Texture)
    | OnBuildingButton Building.Kind
    | BuildUnit Int
    | NewRandom (Float, (Maybe Resource.Kind, List Resource.Kind))
    | SlowTick Time.Posix
