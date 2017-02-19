module ConsoleCommand exposing (parse, ConsoleCommand(..))

import Parser exposing (..)
import Byte


type ConsoleCommand
    = ToggleBreakpoint Int
    | JumpToInstruction Int
    | JumpToMemory Int
    | SetMemoryByteView Byte.Format
    | SetOffsetByteView Byte.Format
    | SetOperandByteView Byte.Format
    | SetRegistersByteView Byte.Format


parse : String -> Result String ConsoleCommand
parse input =
    run
        (oneOf
            [ parseBreakpointCommand
            , parseJumpToInstructionCommand
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
        |= int


parseJumpToInstructionCommand : Parser ConsoleCommand
parseJumpToInstructionCommand =
    succeed JumpToInstruction
        |. keyword "jmpi"
        |. spaces
        |= int


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
