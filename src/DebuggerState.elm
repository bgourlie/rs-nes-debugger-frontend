module DebuggerState exposing (decoder, getByte, getWord, State, Memory)

import Debug
import Json.Decode as Json exposing (Decoder, field)
import Registers exposing (Registers)
import List exposing (drop, take, head)
import Bitwise


type alias State =
    { cycles : Int
    , registers : Registers.Registers
    , screen : Screen
    , memory : Memory
    }


type alias Memory =
    ( Int, List Int )


type alias Screen =
    { width : Int
    , height : Int
    , buffer : String
    }


type MemoryMessage
    = NoChange Int
    | Updated Memory


screenDecoder : Decoder Screen
screenDecoder =
    Json.map3 Screen
        (field "width" Json.int)
        (field "height" Json.int)
        (field "buffer" Json.string)


decoder : Memory -> Decoder State
decoder oldMemory =
    Json.map4 State
        (field "cycles" Json.int)
        (field "registers" Registers.decoder)
        (field "screen" screenDecoder)
        ((field "memory" memoryMessageDecoder)
            |> Json.andThen
                (\memorySnapshot ->
                    case memorySnapshot of
                        NoChange hash ->
                            let
                                ( oldHash, oldMem ) =
                                    oldMemory
                            in
                                if hash == oldHash then
                                    Json.succeed oldMemory
                                else
                                    -- TODO
                                    Json.succeed <| Debug.log "TODO: Stale memory, request latest" oldMemory

                        Updated newMemory ->
                            Json.succeed newMemory
                )
        )


getByte : Int -> Memory -> Int
getByte addr snapshot =
    let
        ( _, memory ) =
            snapshot
    in
        memory
            |> drop addr
            |> take 1
            |> head
            |> Maybe.withDefault 0


getWord : Int -> Memory -> Int
getWord addr snapshot =
    let
        low_byte =
            getByte addr snapshot

        high_byte =
            getByte (addr + 1) snapshot
    in
        Bitwise.or low_byte (Bitwise.shiftLeftBy 8 high_byte)


memoryMessageDecoder : Decoder MemoryMessage
memoryMessageDecoder =
    (field "state" Json.string)
        |> Json.andThen
            (\state ->
                case state of
                    "NoChange" ->
                        (field "hash" Json.int)
                            |> Json.andThen (\hash -> Json.succeed <| NoChange hash)

                    "Updated" ->
                        Json.map2 (,)
                            (field "hash" Json.int)
                            (field "packed_bytes" (Json.list Json.int))
                            |> Json.andThen
                                (\( hash, packedBytes ) ->
                                    let
                                        unpacked =
                                            unpackAll packedBytes
                                    in
                                        Json.succeed <| Updated ( hash, unpacked )
                                )

                    _ ->
                        Json.fail <| "unexpected memory state: " ++ state
            )


unpackAll : List Int -> List Int
unpackAll packedBytes =
    List.concatMap
        (\packedByte ->
            let
                ( byte1, byte2, byte3, byte4 ) =
                    unpack32 packedByte
            in
                [ byte1, byte2, byte3, byte4 ]
        )
        packedBytes


unpack32 : Int -> ( Int, Int, Int, Int )
unpack32 val =
    let
        mask =
            255
    in
        ( Bitwise.and val mask
        , Bitwise.and (Bitwise.shiftRightBy 8 val) mask
        , Bitwise.and (Bitwise.shiftRightBy 16 val) mask
        , Bitwise.and (Bitwise.shiftRightBy 24 val) mask
        )
