module ToggleBreakpoint exposing (request, Message)

--TODO: Move all this into the Breakpoint module

import Task
import Json.Decode
import Json.Decode exposing (Decoder, field)
import Http


type alias Message =
    { offset : Int
    , isSet : Bool
    }


decoder : Decoder Message
decoder =
    Json.Decode.map2 Message
        (field "offset" Json.Decode.int)
        (field "is_set" Json.Decode.bool)


endpoint : Int -> String
endpoint address =
    "http://localhost:9975/toggle_breakpoint/" ++ toString address


request : Int -> (Http.Error -> msg) -> (Message -> msg) -> Cmd msg
request address failHandler successHandler =
    let
        result =
            (\r ->
                case r of
                    Ok r ->
                        successHandler r

                    Err e ->
                        failHandler e
            )
    in
        Http.send result (Http.get (endpoint address) decoder)
