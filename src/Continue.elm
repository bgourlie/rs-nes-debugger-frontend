module Continue exposing (request, Response)

import Task
import Http
import Json.Decode
import Json.Decode exposing (Decoder, (:=))


type alias Response =
    { continued : Bool
    }


decoder : Decoder Response
decoder =
    Json.Decode.object1 Response
        ("continued" := Json.Decode.bool)


endpoint : String
endpoint =
    "http://localhost:9975/continue"


request : (Http.Error -> msg) -> (Response -> msg) -> Cmd msg
request failHandler successHandler =
    Task.perform failHandler successHandler (Http.get decoder endpoint)
