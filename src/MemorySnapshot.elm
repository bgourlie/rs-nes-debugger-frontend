module MemorySnapshot exposing (messageDecoder, MemorySnapshot, Message(NoChange, Updated))

import Http
import Task
import Json.Decode as Json exposing (Decoder, field)
import Bitwise


type alias MemorySnapshot =
    ( Int, List Int )


type Message
    = NoChange Int
    | Updated MemorySnapshot


messageDecoder : Decoder Message
messageDecoder =
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