module ToggleBreakpoint exposing (request, Model)

import Task
import Json.Decode
import Json.Decode exposing (Decoder, (:=))
import Http


type alias Model =
    { address : Int
    , isSet : Bool
    }


decoder : Decoder Model
decoder =
    Json.Decode.object2 Model
        ("address" := Json.Decode.int)
        ("isSet" := Json.Decode.bool)


endpoint : Int -> String
endpoint address =
    "http://localhost:9975/toggle_breakpoint/" ++ toString address


request : Int -> (Http.Error -> msg) -> (Model -> msg) -> Cmd msg
request address failHandler successHandler =
    Task.perform failHandler successHandler (Http.get decoder <| endpoint address)
