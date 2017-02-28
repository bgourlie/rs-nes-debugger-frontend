module DebuggerState exposing (decoder, State)

import Debug
import Json.Decode as Json exposing (Decoder, field)
import Registers exposing (Registers)
import Memory


type alias State =
    { cycles : Int
    , registers : Registers.Registers
    , screen : Screen
    , memory : Memory.Memory
    }


type alias Screen =
    { width : Int
    , height : Int
    , buffer : String
    }


screenDecoder : Decoder Screen
screenDecoder =
    Json.map3 Screen
        (field "width" Json.int)
        (field "height" Json.int)
        (field "buffer" Json.string)


decoder : Memory.Memory -> Decoder State
decoder oldMemory =
    Json.map4 State
        (field "cycles" Json.int)
        (field "registers" Registers.decoder)
        (field "screen" screenDecoder)
        ((field "memory" Memory.messageDecoder)
            |> Json.andThen
                (\memorySnapshot ->
                    case memorySnapshot of
                        Memory.NoChange hash ->
                            let
                                ( oldHash, oldMem ) =
                                    oldMemory
                            in
                                if hash == oldHash then
                                    Json.succeed oldMemory
                                else
                                    -- TODO
                                    Json.succeed <| Debug.log "TODO: Stale memory, request latest" oldMemory

                        Memory.Updated newMemory ->
                            Json.succeed newMemory
                )
        )
