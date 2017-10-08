module Byte exposing (Format(..), asciiValue)

import Char
import Styles


{ id, class, classList } =
    Styles.helpers


type Format
    = Hex
    | Dec
    | Ascii


asciiValue : Int -> String
asciiValue value =
    if value >= 32 && value <= 127 then
        String.fromChar (Char.fromCode value)
    else
        "."
