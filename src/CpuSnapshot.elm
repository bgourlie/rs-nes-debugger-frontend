module CpuSnapshot exposing (decoder, CpuSnapshot)

import Http
import Task
import Debug
import Json.Decode as Json exposing (Decoder, field)
import Registers exposing (Registers)
import Instruction
import MemorySnapshot


type alias CpuSnapshot =
    { cycles : Int
    , registers : Registers.Registers
    , instructions : List Instruction.Instruction
    , memory : MemorySnapshot.MemorySnapshot
    }


decoder : MemorySnapshot.MemorySnapshot -> Decoder CpuSnapshot
decoder oldMemory =
    Json.map4 CpuSnapshot
        (field "cycles" Json.int)
        (field "registers" Registers.decoder)
        (field "instructions" (Json.list Instruction.decoder))
        ((field "memory" MemorySnapshot.messageDecoder)
            |> Json.andThen
                (\memorySnapshot ->
                    case memorySnapshot of
                        MemorySnapshot.NoChange hash ->
                            let
                                ( oldHash, oldMem ) =
                                    oldMemory
                            in
                                if hash == oldHash then
                                    Json.succeed oldMemory
                                else
                                    -- TODO
                                    Json.succeed <| Debug.log "TODO: Stale memory, request latest" oldMemory

                        MemorySnapshot.Updated newMemory ->
                            Json.succeed newMemory
                )
        )
