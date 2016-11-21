module DebuggerCommand exposing (decode, DebuggerCommand, DebuggerCommand(Break), BreakReason(Step, Breakpoint, Trap))

import Json.Decode as Json exposing (Decoder, field)
import CpuSnapshot


type DebuggerCommand
    = Break BreakReason CpuSnapshot.Model


type BreakReason
    = Step
    | Breakpoint
    | Trap


decoder : Decoder DebuggerCommand
decoder =
    (field "command" Json.string) |> Json.andThen decodeByCommand


decodeByCommand : String -> Decoder DebuggerCommand
decodeByCommand cmd =
    case cmd of
        "break" ->
            Json.map2 (,)
                (field "reason" breakReasonDecoder)
                (field "snapshot" CpuSnapshot.decoder)
                |> Json.andThen (\( reason, snapshot ) -> Json.succeed <| Break reason snapshot)

        _ ->
            Json.fail <| "Unknown debugger command: " ++ cmd


breakReasonDecoder : Decoder BreakReason
breakReasonDecoder =
    Json.string
        |> Json.andThen
            (\reason ->
                case reason of
                    "step" ->
                        Json.succeed Step

                    "breakpoint" ->
                        Json.succeed Breakpoint

                    "trap" ->
                        Json.succeed Trap

                    _ ->
                        Json.fail <| "Unexpected break reason: " ++ reason
            )


decode : (String -> msg) -> (DebuggerCommand -> msg) -> String -> msg
decode failHandler successHandler json =
    case Json.decodeString decoder json of
        Ok cmd ->
            successHandler cmd

        Err msg ->
            failHandler msg
