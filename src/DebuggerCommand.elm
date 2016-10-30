module DebuggerCommand exposing (decode, DebuggerCommand, DebuggerCommand(Break))

import Json.Decode exposing (Decoder, (:=))
import Json.Decode as Json


type DebuggerCommand
    = Break


type alias TransportMessage =
    { command : String
    }


transportMessageDecoder : Decoder TransportMessage
transportMessageDecoder =
    Json.object1 TransportMessage
        ("command" := Json.string)


fromJson : String -> Result String DebuggerCommand
fromJson json =
    case Json.decodeString transportMessageDecoder json of
        Ok transportMessage ->
            commandFromTransportMessage transportMessage

        Err message ->
            Err "Malformed transport message"


commandFromTransportMessage : TransportMessage -> Result String DebuggerCommand
commandFromTransportMessage transportMsg =
    case transportMsg.command of
        "Break" ->
            Ok Break

        _ ->
            Err <| "Unexpected debugger command: " ++ transportMsg.command


decode : (String -> msg) -> (DebuggerCommand -> msg) -> String -> msg
decode failHandler successHandler json =
    case fromJson json of
        Ok cmd ->
            successHandler cmd

        Err msg ->
            failHandler msg
