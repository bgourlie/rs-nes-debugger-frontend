module Breakpoints exposing (toggle, isSet, Breakpoints)

import Set exposing (Set)


type alias Breakpoints =
    Set Int


type alias Model a =
    { a | breakpoints : Breakpoints }


toggle : Model a -> Bool -> Int -> Breakpoints
toggle model isSet offset =
    if isSet then
        Set.insert offset model.breakpoints
    else
        Set.remove offset model.breakpoints


isSet : Breakpoints -> Int -> Bool
isSet breakpoints offset =
    Set.member offset breakpoints
