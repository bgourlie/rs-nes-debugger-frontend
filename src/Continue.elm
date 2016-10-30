module Continue exposing (request, Model)

import Task
import Http
import Json.Decode
import Json.Decode exposing (Decoder, (:=))


type alias Model =
    { continued : Bool
    }


decoder : Decoder Model
decoder =
    Json.Decode.object1 Model
        ("continued" := Json.Decode.bool)


endpoint : String
endpoint =
    "http://localhost:9975/continue"


request : (Http.Error -> msg) -> (Model -> msg) -> Cmd msg
request failHandler successHandler =
    Task.perform failHandler successHandler (Http.get decoder endpoint)
