module Msg exposing (Msg (..))

import Key
import Html.Events.Extra.Mouse as Mouse

type Msg
    = Tick Float
    | KeyChange Key.Status String
    | TimeDelta Float
    | MouseDown Mouse.Event
    | MouseMove Int Int

