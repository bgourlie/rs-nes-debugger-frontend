module DebuggerCommand
    exposing
        ( BreakReason(..)
        , CrashReason(..)
        , DebuggerCommand(..)
        , ReceiveResult(..)
        , crashReasonToString
        , decode
        )

import DebuggerState
import Json.Decode as Json exposing (Decoder, field)
import Memory
import ParseInt exposing (toHex)


type ReceiveResult
    = Success DebuggerCommand
    | Error String


type DebuggerCommand
    = Break BreakReason DebuggerState.State
    | Crash CrashReason DebuggerState.State


type BreakReason
    = Step
    | Breakpoint
    | Trap
    | Nmi


type CrashReason
    = InvalidOperation String
    | UnexpectedOpcode Int
    | InvalidVramAccess String Int
    | UnimplementedOperation String


decoder : Memory.Memory -> Decoder DebuggerCommand
decoder oldMemory =
    field "command" Json.string |> Json.andThen (decodeByCommand oldMemory)


decodeByCommand : Memory.Memory -> String -> Decoder DebuggerCommand
decodeByCommand oldMemory cmd =
    case cmd of
        "break" ->
            Json.map2 (,)
                (field "reason" breakReasonDecoder)
                (field "snapshot" <| DebuggerState.decoder oldMemory)
                |> Json.andThen (\( reason, snapshot ) -> Json.succeed (Break reason snapshot))

        "crash" ->
            Json.map2 (,)
                (field "reason" crashReasonDecoder)
                (field "snapshot" <| DebuggerState.decoder oldMemory)
                |> Json.andThen (\( reason, snapshot ) -> Json.succeed (Crash reason snapshot))

        _ ->
            Json.fail <| "Unknown debugger command: " ++ cmd


crashReasonDecoder : Decoder CrashReason
crashReasonDecoder =
    field "type" Json.string
        |> Json.andThen
            (\type_ ->
                case type_ of
                    "invalidOperation" ->
                        field "description" Json.string
                            |> Json.andThen (\description -> Json.succeed (InvalidOperation description))

                    "invalidVramAccess" ->
                        Json.map2 (,)
                            (field "address" Json.int)
                            (field "description" Json.string)
                            |> Json.andThen (\( address, desc ) -> Json.succeed (InvalidVramAccess desc address))

                    "unexpectedOpcode" ->
                        field "opcode" Json.int
                            |> Json.andThen (\opcode -> Json.succeed (UnexpectedOpcode opcode))

                    "unimplementedOperation" ->
                        field "description" Json.string
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


decode : Memory.Memory -> (ReceiveResult -> msg) -> String -> msg
decode oldMemory handler json =
    case Json.decodeString (decoder oldMemory) json of
        Ok cmd ->
            handler <| Success cmd

        Err msg ->
            handler <| Error msg


crashReasonToString : CrashReason -> String
crashReasonToString reason =
    case reason of
        InvalidOperation description ->
            "Invalid Operation (" ++ description ++ ")"

        UnexpectedOpcode opcode ->
            "Unexpected Opcode (0x" ++ String.padLeft 2 '0' (toHex opcode) ++ ")"

        InvalidVramAccess description address ->
            "Invalid VRAM access [0x" ++ String.padLeft 2 '0' (toHex address) ++ "]: " ++ description

        UnimplementedOperation description ->
            "Unimplemented operation (" ++ description ++ ")"
