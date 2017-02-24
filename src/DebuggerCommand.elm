module DebuggerCommand exposing (crashReasonToString, decode, DebuggerCommand, DebuggerCommand(..), BreakReason(..), CrashReason(..))

import Json.Decode as Json exposing (Decoder, field)
import MemorySnapshot
import CpuSnapshot
import ParseInt exposing (toHex)


type DebuggerCommand
    = Break BreakReason CpuSnapshot.CpuSnapshot
    | Crash CrashReason CpuSnapshot.CpuSnapshot


type BreakReason
    = Step
    | Breakpoint
    | Trap
    | Nmi


type CrashReason
    = InvalidOperation String
    | UnexpectedOpcode Int
    | InvalidVramAccess Int
    | UnimplementedOperation String


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
                |> Json.andThen (\( reason, snapshot ) -> Json.succeed (Break reason snapshot))

        "crash" ->
            Json.map2 (,)
                (field "reason" crashReasonDecoder)
                (field "snapshot" <| CpuSnapshot.decoder oldMemory)
                |> Json.andThen (\( reason, snapshot ) -> Json.succeed (Crash reason snapshot))

        _ ->
            Json.fail <| "Unknown debugger command: " ++ cmd


crashReasonDecoder : Decoder CrashReason
crashReasonDecoder =
    (field "type" Json.string)
        |> Json.andThen
            (\type_ ->
                case type_ of
                    "invalidOperation" ->
                        (field "description" Json.string)
                            |> Json.andThen (\description -> Json.succeed (InvalidOperation description))

                    "invalidVramAccess" ->
                        (field "address" Json.int)
                            |> Json.andThen (\address -> Json.succeed (InvalidVramAccess address))

                    "unexpectedOpcode" ->
                        (field "opcode" Json.int)
                            |> Json.andThen (\opcode -> Json.succeed (UnexpectedOpcode opcode))

                    "unimplementedOperation" ->
                        (field "description" Json.string)
                            |> Json.andThen (\description -> Json.succeed (UnimplementedOperation description))

                    _ ->
                        Json.fail <| "Unexpected crash reason: " ++ type_
            )


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

                    "nmi" ->
                        Json.succeed Nmi

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


crashReasonToString : CrashReason -> String
crashReasonToString reason =
    case reason of
        InvalidOperation description ->
            "Invalid Operation (" ++ description ++ ")"

        UnexpectedOpcode opcode ->
            "Unexpected Opcode (0x" ++ String.padLeft 2 '0' (toHex opcode) ++ ")"

        InvalidVramAccess address ->
            "Invalid VRAM access (0x" ++ String.padLeft 2 '0' (toHex address) ++ ")"

        UnimplementedOperation description ->
            "Unimplemented operation (" ++ description ++ ")"
