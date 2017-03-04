module Continue exposing (request, Result(..))

import Http
import Json.Decode exposing (Decoder, field)


type Result
    = Success SuccessModel
    | Error String


type alias SuccessModel =
    { continued : Bool
    }


decoder : Decoder SuccessModel
decoder =
    Json.Decode.map SuccessModel
        (field "continued" Json.Decode.bool)


endpoint : String
endpoint =
    "http://localhost:9975/continue"


request : (Result -> msg) -> ( a, Cmd msg ) -> ( a, Cmd msg )
request handler ( inputModel, inputCmd ) =
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
        ( inputModel, Cmd.batch [ inputCmd, Http.send result (Http.get endpoint decoder) ] )
