module DebuggerCommand exposing (decode, DebuggerCommand, DebuggerCommand(Break), BreakReason(Step, Breakpoint, Trap))

import Json.Decode as Json exposing (Decoder, field)
import MemorySnapshot
import CpuSnapshot


type DebuggerCommand
    = Break BreakReason CpuSnapshot.CpuSnapshot


type BreakReason
    = Step
    | Breakpoint
    | Trap


decoder : MemorySnapshot.MemorySnapshot -> Decoder DebuggerCommand
decoder oldMemory =
    (field "command" Json.string) |> Json.andThen (decodeByCommand oldMemory)


decodeByCommand : MemorySnapshot.MemorySnapshot -> String -> Decoder DebuggerCommand
decodeByCommand oldMemory cmd =
    case cmd of
        "break" ->
            Json.map2 (,)
                (field "reason" breakReasonDecoder)
                (field "snapshot" <| CpuSnapshot.decoder oldMemory)
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


decode : MemorySnapshot.MemorySnapshot -> (String -> msg) -> (DebuggerCommand -> msg) -> String -> msg
decode oldMemory failHandler successHandler json =
    case Json.decodeString (decoder oldMemory) json of
        Ok cmd ->
            successHandler cmd

        Err msg ->
            failHandler msg
