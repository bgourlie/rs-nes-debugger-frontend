module ToggleBreakpoint exposing (responseDecoder, Response, endpoint)

import Task exposing (Task)
import Http
import Json.Decode
import Json.Decode exposing (Decoder, (:=))


type alias Response =
    { address : Int
    , isSet : Bool
    }


responseDecoder : Decoder Response
responseDecoder =
    Json.Decode.object2 Response
        ("address" := Json.Decode.int)
        ("isSet" := Json.Decode.bool)


endpoint : Int -> String
endpoint address =
    "http://localhost:9975/toggle_breakpoint/" ++ toString address
