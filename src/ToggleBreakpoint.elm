module ToggleBreakpoint exposing (Result(..), request)

import Http
import Json.Decode exposing (Decoder, field)


endpoint : Int -> String
endpoint address =
    "http://localhost:9975/toggle_breakpoint/" ++ toString address


type Result
    = Success ResponseModel
    | Error String


type alias ResponseModel =
    { offset : Int
    , isSet : Bool
    }


responseModelDecoder : Decoder ResponseModel
responseModelDecoder =
    Json.Decode.map2 ResponseModel
        (field "offset" Json.Decode.int)
        (field "is_set" Json.Decode.bool)


request : Int -> (Result -> msg) -> ( a, Cmd msg ) -> ( a, Cmd msg )
request address handler ( inputModel, inputCmd ) =
    let
        result =
            \r ->
                case r of
                    Ok r ->
                        handler <| Success r

                    Err e ->
                        handler <| Error (toString e)
    in
    ( inputModel, Cmd.batch [ inputCmd, Http.send result (Http.get (endpoint address) responseModelDecoder) ] )
