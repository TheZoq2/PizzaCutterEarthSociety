module Msg exposing (Msg (..))

import Key

type Msg
    = Tick Float
    | KeyChange Key.Status String
    | TimeDelta Float
    | MouseDown Int Int

