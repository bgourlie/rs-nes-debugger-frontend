module Memory exposing (messageDecoder, Memory, MemoryMessage(..))

import Bitwise
import Json.Decode as Json exposing (Decoder, field)
import ByteArray


type alias Memory =
    ( Int, ByteArray.ByteArray )


type MemoryMessage
    = NoChange Int
    | Updated Memory


messageDecoder : Decoder MemoryMessage
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
                            (field "base64" Json.string)
                            |> Json.andThen
                                (\( hash, base64 ) ->
                                    case ByteArray.fromBase64 base64 of
                                        Ok byteArray ->
                                            Json.succeed <| Updated ( hash, byteArray )

                                        Err err ->
                                            Json.fail "Unable to decode base64-encoded memory"
                                )

                    _ ->
                        Json.fail <| "unexpected memory state: " ++ state
            )
