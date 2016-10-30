module ToggleBreakpoint exposing (request, Response)

import Task
import Json.Decode
import Json.Decode exposing (Decoder, (:=))
import Http


type alias Response =
    { address : Int
    , isSet : Bool
    }


decoder : Decoder Response
decoder =
    Json.Decode.object2 Response
        ("address" := Json.Decode.int)
        ("isSet" := Json.Decode.bool)


endpoint : Int -> String
endpoint address =
    "http://localhost:9975/toggle_breakpoint/" ++ toString address


request : Int -> (Http.Error -> msg) -> (Response -> msg) -> Cmd msg
request address failHandler successHandler =
    Task.perform failHandler successHandler (Http.get decoder <| endpoint address)
