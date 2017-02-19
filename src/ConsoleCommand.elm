module ConsoleCommand exposing (parse, ConsoleCommand(..))

import Parser exposing (..)


type ConsoleCommand
    = ToggleBreakpoint Int
    | JumpToInstruction Int
    | JumpToMemory Int


parse : String -> Result String ConsoleCommand
parse input =
    Parser.run
        (Parser.oneOf
            [ parseBreakpointCommand
            , parseJumpToInstructionCommand
            , parseJumpToMemoryCommand
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
        |= int


spaces : Parser ()
spaces =
    ignoreWhile (\char -> char == ' ')
