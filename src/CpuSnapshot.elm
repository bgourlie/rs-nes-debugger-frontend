module CpuSnapshot exposing (request, Response, Registers)

import Http
import Task
import Json.Decode exposing (Decoder, (:=))
import Json.Decode as Json


endpoint =
    "http://localhost:9975/snapshot"


type alias Response =
    { cycles : Int
    , registers : Registers
    , memory : List Int
    }


type alias Registers =
    { acc : Int
    , x : Int
    , y : Int
    , pc : Int
    , sp : Int
    , stat : Int
    }


decoder : Decoder Response
decoder =
    Json.object3 Response
        ("cycles" := Json.int)
        ("registers" := registersDecoder)
        ("memory" := (Json.list Json.Decode.int))


registersDecoder : Decoder Registers
registersDecoder =
    Json.object6 Registers
        ("acc" := Json.int)
        ("x" := Json.int)
        ("y" := Json.int)
        ("pc" := Json.int)
        ("sp" := Json.int)
        ("stat" := Json.int)


request : (Http.Error -> msg) -> (Response -> msg) -> Cmd msg
request failHandler successHandler =
    Task.perform failHandler successHandler (Http.get decoder endpoint)
