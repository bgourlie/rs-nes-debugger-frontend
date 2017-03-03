module Step exposing (request, Result(..))

import Http
import Json.Decode
import Json.Decode exposing (Decoder, field)


type alias SuccessModel =
    { stepped : Bool
    }


type Result
    = Success SuccessModel
    | Error String


decoder : Decoder SuccessModel
decoder =
    Json.Decode.map SuccessModel
        (field "stepped" Json.Decode.bool)


endpoint : String
endpoint =
    "http://localhost:9975/step"


request : (Result -> msg) -> Cmd msg
request handler =
    let
        result =
            (\r ->
                case r of
                    Ok r ->
                        handler <| Success r

                    Err e ->
                        handler <| Error (toString e)
            )
    in
        Http.send result (Http.get endpoint decoder)
