module DebuggerCommand exposing (decode, DebuggerCommand, DebuggerCommand(Break))

import Json.Decode as Json exposing (Decoder, (:=))
import CpuSnapshot


type DebuggerCommand
    = Break CpuSnapshot.Model


decoder : Decoder DebuggerCommand
decoder =
    ("command" := Json.string) `Json.andThen` decodeByCommand


decodeByCommand : String -> Decoder DebuggerCommand
decodeByCommand cmd =
    case cmd of
        "break" ->
            ("value" := CpuSnapshot.decoder) `Json.andThen` (\snapshot -> Json.succeed <| Break snapshot)

        _ ->
            Json.fail <| "Unknown debugger command: " ++ cmd


decode : (String -> msg) -> (DebuggerCommand -> msg) -> String -> msg
decode failHandler successHandler json =
    case Json.decodeString decoder json of
        Ok cmd ->
            successHandler cmd

        Err msg ->
            failHandler msg
