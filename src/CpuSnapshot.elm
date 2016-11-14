module CpuSnapshot exposing (decoder, Model)

import Http
import Task
import Json.Decode exposing (Decoder, (:=))
import Json.Decode as Json
import Registers exposing (Model)
import Instruction


type alias Model =
    { cycles : Int
    , registers : Registers.Model
    , instructions : List Instruction.Model
    }


decoder : Decoder Model
decoder =
    Json.object3 Model
        ("cycles" := Json.int)
        ("registers" := Registers.decoder)
        ("instructions" := (Json.list Instruction.decoder))
