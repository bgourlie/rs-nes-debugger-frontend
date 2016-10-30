module CpuSnapshot exposing (request, Model)

import Http
import Task
import Json.Decode exposing (Decoder, (:=))
import Json.Decode as Json
import Registers exposing (Model)


endpoint =
    "http://localhost:9975/snapshot"


type alias Model =
    { cycles : Int
    , registers : Registers.Model
    , memory : List Int
    }


decoder : Decoder Model
decoder =
    Json.object3 Model
        ("cycles" := Json.int)
        ("registers" := Registers.decoder)
        ("memory" := (Json.list Json.Decode.int))


request : (Http.Error -> msg) -> (Model -> msg) -> Cmd msg
request failHandler successHandler =
    Task.perform failHandler successHandler (Http.get decoder endpoint)
