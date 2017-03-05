module ConsoleCommand exposing (parse, ConsoleCommand(..), BreakpointType(..))

import Parser exposing (..)
import Byte


type ConsoleCommand
    = ToggleBreakpoint BreakpointType
    | JumpToMemory Int
    | SetDisassembleOffset Int
    | SetMemoryByteView Byte.Format
    | SetOffsetByteView Byte.Format
    | SetOperandByteView Byte.Format
    | SetRegistersByteView Byte.Format


type BreakpointType
    = Offset Int
    | Nmi


parse : String -> Result String ConsoleCommand
parse input =
    run
        (oneOf
            [ parseBreakpointCommand
            , parseSetDisassembleOffsetCommand
            , parseJumpToMemoryCommand
            , parseSetMemoryByteView
            , parseSetOffsetByteView
            , parseSetOperandByteView
            , parseSetRegistersByteView
            ]
        )
        input
        |> Result.mapError (\_ -> "An error occurred while parsing the command")


parseBreakpointCommand : Parser ConsoleCommand
parseBreakpointCommand =
    succeed ToggleBreakpoint
        |. keyword "bp"
        |. spaces
        |= (oneOf
                [ int |> andThen (\offset -> succeed (Offset offset))
                , (keyword "nmi") |> andThen (\_ -> succeed Nmi)
                ]
           )


parseJumpToMemoryCommand : Parser ConsoleCommand
parseJumpToMemoryCommand =
    succeed JumpToMemory
        |. keyword "jmpm"
        |. spaces
        |= (oneOf
                [ int
                , (keyword "stack") |> andThen (\_ -> (succeed 0x0100))
                ]
           )


parseSetDisassembleOffsetCommand : Parser ConsoleCommand
parseSetDisassembleOffsetCommand =
    succeed SetDisassembleOffset
        |. keyword "sdo"
        |. spaces
        |= int


parseSetMemoryByteView : Parser ConsoleCommand
parseSetMemoryByteView =
    succeed SetMemoryByteView
        |. keyword "memview"
        |. spaces
        |= parseByteFormat


parseSetOffsetByteView : Parser ConsoleCommand
parseSetOffsetByteView =
    succeed SetOffsetByteView
        |. keyword "offsetview"
        |. spaces
        |= parseByteFormat


parseSetRegistersByteView : Parser ConsoleCommand
parseSetRegistersByteView =
    succeed SetRegistersByteView
        |. keyword "regview"
        |. spaces
        |= parseByteFormat


parseSetOperandByteView : Parser ConsoleCommand
parseSetOperandByteView =
    succeed SetOperandByteView
        |. keyword "opview"
        |. spaces
        |= parseByteFormat


parseByteFormat : Parser Byte.Format
parseByteFormat =
    (oneOf
        [ (keyword "hex") |> andThen (\_ -> (succeed Byte.Hex))
        , (keyword "dec") |> andThen (\_ -> (succeed Byte.Dec))
        , (keyword "ascii") |> andThen (\_ -> (succeed Byte.Ascii))
        ]
    )


spaces : Parser ()
spaces =
    ignoreWhile (\char -> char == ' ')
