module ToggleNmiBreakpoint exposing (Result(..), request)

import Http
import Json.Decode as Json


type Result
    = Success ResponseModel
    | Error String


type alias ResponseModel =
    { isSet : Bool
    }


responseModelDecoder : Json.Decoder ResponseModel
responseModelDecoder =
    Json.map ResponseModel
        (Json.field "is_set" Json.bool)


endpoint : String
endpoint =
    "http://localhost:9975/toggle_break_on_nmi"


request : (Result -> msg) -> Cmd msg
request handler =
    let
        result =
            \r ->
                case r of
                    Ok r ->
                        handler <| Success r

                    Err e ->
                        handler <| Error (toString e)
    in
    Http.send result (Http.get endpoint responseModelDecoder)
