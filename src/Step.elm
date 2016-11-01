module Step exposing (request, Model)

import Task
import Http
import Json.Decode
import Json.Decode exposing (Decoder, (:=))


type alias Model =
    { stepped : Bool
    }


decoder : Decoder Model
decoder =
    Json.Decode.object1 Model
        ("stepped" := Json.Decode.bool)


endpoint : String
endpoint =
    "http://localhost:9975/step"


request : (Http.Error -> msg) -> (Model -> msg) -> Cmd msg
request failHandler successHandler =
    Task.perform failHandler successHandler (Http.get decoder endpoint)
