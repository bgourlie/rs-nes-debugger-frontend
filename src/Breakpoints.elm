module Breakpoints
    exposing
        ( toggleBreakpoint
        , isSet
        , toggleBreakpointRequest
        , toggleBreakOnNmiRequest
        , Breakpoints
        , ToggleBreakpointResponse
        , ToggleBreakOnNmiResponse
        )

import Set exposing (Set)
import Json.Decode
import Json.Decode exposing (Decoder, field)
import Http


type alias Breakpoints =
    Set Int


type alias Model a =
    { a | breakpoints : Breakpoints }


toggleBreakpoint : Model a -> Bool -> Int -> Breakpoints
toggleBreakpoint model isSet offset =
    if isSet then
        Set.insert offset model.breakpoints
    else
        Set.remove offset model.breakpoints


isSet : Breakpoints -> Int -> Bool
isSet breakpoints offset =
    Set.member offset breakpoints


type alias ToggleBreakpointResponse =
    { offset : Int
    , isSet : Bool
    }


type alias ToggleBreakOnNmiResponse =
    { isSet : Bool
    }


toggleBreakpointResponseDecoder : Decoder ToggleBreakpointResponse
toggleBreakpointResponseDecoder =
    Json.Decode.map2 ToggleBreakpointResponse
        (field "offset" Json.Decode.int)
        (field "is_set" Json.Decode.bool)


toggleBreakOnNmiResponseDecoder : Decoder ToggleBreakOnNmiResponse
toggleBreakOnNmiResponseDecoder =
    Json.Decode.map ToggleBreakOnNmiResponse
        (field "is_set" Json.Decode.bool)


toggleBreakpointEndpoint : Int -> String
toggleBreakpointEndpoint address =
    "http://localhost:9975/toggle_breakpoint/" ++ toString address


toggleBreakOnNmiEndpoint : String
toggleBreakOnNmiEndpoint =
    "http://localhost:9975/toggle_break_on_nmi"


toggleBreakpointRequest : Int -> (Http.Error -> msg) -> (ToggleBreakpointResponse -> msg) -> Cmd msg
toggleBreakpointRequest address failHandler successHandler =
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
        Http.send result (Http.get (toggleBreakpointEndpoint address) toggleBreakpointResponseDecoder)


toggleBreakOnNmiRequest : (Http.Error -> msg) -> (ToggleBreakOnNmiResponse -> msg) -> Cmd msg
toggleBreakOnNmiRequest failHandler successHandler =
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
        Http.send result (Http.get toggleBreakOnNmiEndpoint toggleBreakOnNmiResponseDecoder)
