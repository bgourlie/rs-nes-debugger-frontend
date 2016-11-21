module Continue exposing (request, Model)

import Task
import Http
import Json.Decode exposing (Decoder, field)


type alias Model =
    { continued : Bool
    }


decoder : Decoder Model
decoder =
    Json.Decode.map Model
        (field "continued" Json.Decode.bool)


endpoint : String
endpoint =
    "http://localhost:9975/continue"


request : (Http.Error -> msg) -> (Model -> msg) -> Cmd msg
request failHandler successHandler =
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
        Http.send result (Http.get endpoint decoder)
