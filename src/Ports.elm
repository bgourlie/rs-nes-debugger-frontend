port module Ports exposing (scrollElementIntoViewCommand, receiveScrollEventsForCommand, scrollEvent, mapScrollEvent, ScrollEvent)

import Json.Encode
import Json.Decode


{-| Given a class name, scroll the first element with that class into view.
-}
port scrollElementIntoViewCommand : String -> Cmd msg


{-| Will report scroll events for the element with the given id back through the scrollEvents port.
-}
port receiveScrollEventsForCommand : String -> Cmd msg


{-| Report scroll events.
-}
port scrollEvent : (Json.Encode.Value -> msg) -> Sub msg


type alias ScrollEvent =
    { elementId : String
    , scrollPosition : Float
    }


mapScrollEvent : (String -> msg) -> (ScrollEvent -> msg) -> Json.Encode.Value -> msg
mapScrollEvent failHandler successHandler value =
    let
        decoder =
            Json.Decode.map2 ScrollEvent
                (Json.Decode.field "elementId" Json.Decode.string)
                (Json.Decode.field "scrollPosition" Json.Decode.float)
    in
        case Json.Decode.decodeValue decoder value of
            Ok scrollEvent ->
                successHandler scrollEvent

            Err decodeError ->
                failHandler decodeError
