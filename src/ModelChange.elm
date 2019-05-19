module ModelChange exposing (ModelChange(..))

import Resource

type ModelChange
    = AddResource Resource.Kind
    | ReduceResourceStock Int
