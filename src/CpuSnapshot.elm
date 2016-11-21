module CpuSnapshot exposing (decoder, Model)

import Http
import Task
import Json.Decode as Json exposing (Decoder, field)
import Registers exposing (Model)
import Instruction


type alias Model =
    { cycles : Int
    , registers : Registers.Model
    , instructions : List Instruction.Model
    }


decoder : Decoder Model
decoder =
    Json.map3 Model
        (field "cycles" Json.int)
        (field "registers" Registers.decoder)
        (field "instructions" (Json.list Instruction.decoder))
