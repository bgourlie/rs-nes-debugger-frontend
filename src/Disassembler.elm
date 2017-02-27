module Disassembler exposing (disassemble, Instruction(..))

import ByteArray exposing (ByteArray)
import AddressingMode exposing (AddressingMode(..))


type Instruction
    = Known Int String AddressingMode
    | Undefined Int


disassemble : Int -> Int -> ByteArray -> List Instruction
disassemble startOffset instructionsToDecode memory =
    disassemble_ startOffset instructionsToDecode memory []
        |> List.reverse


disassemble_ : Int -> Int -> ByteArray -> List Instruction -> List Instruction
disassemble_ offset remainingInstructions memory inputList =
    case remainingInstructions of
        0 ->
            inputList

        _ ->
            case ByteArray.get offset memory of
                Nothing ->
                    inputList

                Just byte ->
                    let
                        ( bytesRead, instruction ) =
                            case byte of
                                0x00 ->
                                    ( 1, Known offset "BRK" Implied )

                                0x18 ->
                                    ( 1, Known offset "CLC" Implied )

                                0x38 ->
                                    ( 1, Known offset "SEC" Implied )

                                0x58 ->
                                    ( 1, Known offset "CLI" Implied )

                                0x78 ->
                                    ( 1, Known offset "SEI" Implied )

                                0xB8 ->
                                    ( 1, Known offset "CLV" Implied )

                                0xD8 ->
                                    ( 1, Known offset "CLD" Implied )

                                0xF8 ->
                                    ( 1, Known offset "SED" Implied )

                                0xEA ->
                                    ( 1, Known offset "NOP" Implied )

                                0xAA ->
                                    ( 1, Known offset "TAX" Implied )

                                0x8A ->
                                    ( 1, Known offset "TXA" Implied )

                                0xCA ->
                                    ( 1, Known offset "DEX" Implied )

                                0xE8 ->
                                    ( 1, Known offset "INX" Implied )

                                0xA8 ->
                                    ( 1, Known offset "TAY" Implied )

                                0x98 ->
                                    ( 1, Known offset "TYA" Implied )

                                0x88 ->
                                    ( 1, Known offset "DEY" Implied )

                                0xC8 ->
                                    ( 1, Known offset "INY" Implied )

                                _ ->
                                    ( 1, Undefined offset )

                        nextOffset =
                            offset + bytesRead

                        outputList =
                            instruction :: inputList
                    in
                        disassemble_ nextOffset (remainingInstructions - 1) memory outputList
