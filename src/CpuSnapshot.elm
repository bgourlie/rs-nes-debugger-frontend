module CpuSnapshot exposing (CpuSnapshot, Registers, cpuSnapshotDecoder)

import Json.Decode exposing (Decoder, (:=))
import Json.Decode as Json


type alias CpuSnapshot =
    { cycles : Int
    , registers : Registers
    , memory : List Int
    }


type alias Registers =
    { acc : Int
    , x : Int
    , y : Int
    , pc : Int
    , sp : Int
    , stat : Int
    }


cpuSnapshotDecoder : Decoder CpuSnapshot
cpuSnapshotDecoder =
    Json.object3 CpuSnapshot
        ("cycles" := Json.int)
        ("registers" := registersDecoder)
        ("memory" := (Json.list Json.Decode.int))


registersDecoder : Decoder Registers
registersDecoder =
    Json.object6 Registers
        ("acc" := Json.int)
        ("x" := Json.int)
        ("y" := Json.int)
        ("pc" := Json.int)
        ("sp" := Json.int)
        ("stat" := Json.int)
