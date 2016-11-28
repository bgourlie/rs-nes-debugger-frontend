module CpuSnapshot exposing (decoder, Model)

import Http
import Task
import Json.Decode as Json exposing (Decoder, field)
import Registers exposing (Registers)
import Instruction


type alias Model =
    { cycles : Int
    , registers : Registers.Registers
    , instructions : List Instruction.Instruction
    , memory : List Int
    }


decoder : Decoder Model
decoder =
    Json.map4 Model
        (field "cycles" Json.int)
        (field "registers" Registers.decoder)
        (field "instructions" (Json.list Instruction.decoder))
        (field "memory" (Json.list Json.int))
