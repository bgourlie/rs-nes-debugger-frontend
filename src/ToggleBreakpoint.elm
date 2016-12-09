module ToggleBreakpoint exposing (request, Model)

--TODO: Move all this into the Breakpoint module

import Task
import Json.Decode
import Json.Decode exposing (Decoder, field)
import Http


type alias Model =
    { offset : Int
    , isSet : Bool
    }


decoder : Decoder Model
decoder =
    Json.Decode.map2 Model
        (field "offset" Json.Decode.int)
        (field "is_set" Json.Decode.bool)


endpoint : Int -> String
endpoint address =
    "http://localhost:9975/toggle_breakpoint/" ++ toString address


request : Int -> (Http.Error -> msg) -> (Model -> msg) -> Cmd msg
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
