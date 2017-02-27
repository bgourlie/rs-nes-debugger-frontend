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
                        decoded =
                            case byte of
                                0x69 ->
                                    immediate "ADC" offset memory

                                0x65 ->
                                    zeroPage "ADC" offset memory

                                0x75 ->
                                    zeroPageX "ADC" offset memory

                                0x6D ->
                                    absolute "ADC" offset memory

                                0x7D ->
                                    absoluteX "ADC" offset memory

                                0x79 ->
                                    absoluteY "ADC" offset memory

                                0x61 ->
                                    indexedIndirect "ADC" offset memory

                                0x71 ->
                                    indirectIndexed "ADC" offset memory

                                0x29 ->
                                    immediate "AND" offset memory

                                0x25 ->
                                    zeroPage "AND" offset memory

                                0x35 ->
                                    zeroPageX "AND" offset memory

                                0x2D ->
                                    absolute "AND" offset memory

                                0x3D ->
                                    absoluteX "AND" offset memory

                                0x39 ->
                                    absoluteY "AND" offset memory

                                0x21 ->
                                    indexedIndirect "AND" offset memory

                                0x31 ->
                                    indirectIndexed "AND" offset memory

                                0x0A ->
                                    accumulator "ASL" offset

                                0x06 ->
                                    zeroPage "ASL" offset memory

                                0x16 ->
                                    zeroPageX "ASL" offset memory

                                0x0E ->
                                    absolute "ASL" offset memory

                                0x1E ->
                                    absoluteX "ASL" offset memory

                                0x24 ->
                                    zeroPage "BIT" offset memory

                                0x2C ->
                                    absolute "BIT" offset memory

                                0x10 ->
                                    relative "BPL" offset memory

                                0x30 ->
                                    relative "BMI" offset memory

                                0x50 ->
                                    relative "BVC" offset memory

                                0x70 ->
                                    relative "BVS" offset memory

                                0x90 ->
                                    relative "BCC" offset memory

                                0xB0 ->
                                    relative "BCS" offset memory

                                0xD0 ->
                                    relative "BNE" offset memory

                                0xF0 ->
                                    relative "BEQ" offset memory

                                0x00 ->
                                    implied "BRK" offset

                                0xC9 ->
                                    immediate "CMP" offset memory

                                0xC5 ->
                                    zeroPage "CMP" offset memory

                                0xD5 ->
                                    zeroPageX "CMP" offset memory

                                0xCD ->
                                    absolute "CMP" offset memory

                                0xDD ->
                                    absoluteX "CMP" offset memory

                                0xD9 ->
                                    absoluteY "CMP" offset memory

                                0xC1 ->
                                    indexedIndirect "CMP" offset memory

                                0xD1 ->
                                    indirectIndexed "CMP" offset memory

                                0xE0 ->
                                    immediate "CPX" offset memory

                                0xE4 ->
                                    zeroPage "CPX" offset memory

                                0xEC ->
                                    absolute "CPX" offset memory

                                0xC0 ->
                                    immediate "CPY" offset memory

                                0xC4 ->
                                    zeroPage "CPY" offset memory

                                0xCC ->
                                    absolute "CPY" offset memory

                                0xC6 ->
                                    zeroPage "DEC" offset memory

                                0xD6 ->
                                    zeroPageX "DEC" offset memory

                                0xCE ->
                                    absolute "DEC" offset memory

                                0xDE ->
                                    absoluteX "DEC" offset memory

                                0x49 ->
                                    immediate "EOR" offset memory

                                0x45 ->
                                    zeroPage "EOR" offset memory

                                0x55 ->
                                    zeroPageX "EOR" offset memory

                                0x4D ->
                                    absolute "EOR" offset memory

                                0x5D ->
                                    absoluteX "EOR" offset memory

                                0x59 ->
                                    absoluteY "EOR" offset memory

                                0x41 ->
                                    indexedIndirect "EOR" offset memory

                                0x51 ->
                                    indirectIndexed "EOR" offset memory

                                0x18 ->
                                    implied "CLC" offset

                                0x38 ->
                                    implied "SEC" offset

                                0x58 ->
                                    implied "CLI" offset

                                0x78 ->
                                    implied "SEI" offset

                                0xB8 ->
                                    implied "CLV" offset

                                0xD8 ->
                                    implied "CLD" offset

                                0xF8 ->
                                    implied "SED" offset

                                0xE6 ->
                                    zeroPage "INC" offset memory

                                0xF6 ->
                                    zeroPageX "INC" offset memory

                                0xEE ->
                                    absolute "INC" offset memory

                                0xFE ->
                                    absoluteX "INC" offset memory

                                0x4C ->
                                    absolute "JMP" offset memory

                                0x6C ->
                                    indirect "JMP" offset memory

                                0x20 ->
                                    absolute "JSR" offset memory

                                0xA9 ->
                                    immediate "LDA" offset memory

                                0xA5 ->
                                    zeroPage "LDA" offset memory

                                0xB5 ->
                                    zeroPageX "LDA" offset memory

                                0xAD ->
                                    absolute "LDA" offset memory

                                0xBD ->
                                    absoluteX "LDA" offset memory

                                0xB9 ->
                                    absoluteY "LDA" offset memory

                                0xA1 ->
                                    indexedIndirect "LDA" offset memory

                                0xB1 ->
                                    indirectIndexed "LDA" offset memory

                                0xA2 ->
                                    immediate "LDX" offset memory

                                0xA6 ->
                                    zeroPage "LDX" offset memory

                                0xB6 ->
                                    zeroPageY "LDX" offset memory

                                0xAE ->
                                    absolute "LDX" offset memory

                                0xBE ->
                                    absoluteY "LDX" offset memory

                                0xA0 ->
                                    immediate "LDY" offset memory

                                0xA4 ->
                                    zeroPage "LDY" offset memory

                                0xB4 ->
                                    zeroPageX "LDY" offset memory

                                0xAC ->
                                    absolute "LDY" offset memory

                                0xBC ->
                                    absoluteX "LDY" offset memory

                                0x4A ->
                                    accumulator "LSR" offset

                                0x46 ->
                                    zeroPage "LSR" offset memory

                                0x56 ->
                                    zeroPageX "LSR" offset memory

                                0x4E ->
                                    absolute "LSR" offset memory

                                0x5E ->
                                    absoluteX "LSR" offset memory

                                0xEA ->
                                    implied "NOP" offset

                                0x09 ->
                                    immediate "ORA" offset memory

                                0x05 ->
                                    zeroPage "ORA" offset memory

                                0x15 ->
                                    zeroPageX "ORA" offset memory

                                0x0D ->
                                    absolute "ORA" offset memory

                                0x1D ->
                                    absoluteX "ORA" offset memory

                                0x19 ->
                                    absoluteY "ORA" offset memory

                                0x01 ->
                                    indexedIndirect "ORA" offset memory

                                0x11 ->
                                    indirectIndexed "ORA" offset memory

                                0xAA ->
                                    implied "TAX" offset

                                0x8A ->
                                    implied "TXA" offset

                                0xCA ->
                                    implied "DEX" offset

                                0xE8 ->
                                    implied "INX" offset

                                0xA8 ->
                                    implied "TAY" offset

                                0x98 ->
                                    implied "TYA" offset

                                0x88 ->
                                    implied "DEY" offset

                                0xC8 ->
                                    implied "INY" offset

                                0x2A ->
                                    accumulator "ROL" offset

                                0x26 ->
                                    zeroPage "ROL" offset memory

                                0x36 ->
                                    zeroPageX "ROL" offset memory

                                0x2E ->
                                    absolute "ROL" offset memory

                                0x3E ->
                                    absoluteX "ROL" offset memory

                                0x6A ->
                                    accumulator "ROR" offset

                                0x66 ->
                                    zeroPage "ROR" offset memory

                                0x76 ->
                                    zeroPageX "ROR" offset memory

                                0x6E ->
                                    absolute "ROR" offset memory

                                0x7E ->
                                    absoluteX "ROR" offset memory

                                0x40 ->
                                    implied "RTI" offset

                                0x60 ->
                                    implied "RTS" offset

                                0xE9 ->
                                    immediate "SBC" offset memory

                                0xE5 ->
                                    zeroPage "SBC" offset memory

                                0xF5 ->
                                    zeroPageX "SBC" offset memory

                                0xED ->
                                    absolute "SBC" offset memory

                                0xFD ->
                                    absoluteX "SBC" offset memory

                                0xF9 ->
                                    absoluteY "SBC" offset memory

                                0xE1 ->
                                    indexedIndirect "SBC" offset memory

                                0xF1 ->
                                    indirectIndexed "SBC" offset memory

                                0x85 ->
                                    zeroPage "STA" offset memory

                                0x95 ->
                                    zeroPageX "STA" offset memory

                                0x8D ->
                                    absolute "STA" offset memory

                                0x9D ->
                                    absoluteX "STA" offset memory

                                0x99 ->
                                    absoluteY "STA" offset memory

                                0x81 ->
                                    indexedIndirect "STA" offset memory

                                0x91 ->
                                    indirectIndexed "STA" offset memory

                                0x9A ->
                                    implied "TXS" offset

                                0xBA ->
                                    implied "TSX" offset

                                0x48 ->
                                    implied "PHA" offset

                                0x68 ->
                                    implied "PLA" offset

                                0x08 ->
                                    implied "PHP" offset

                                0x28 ->
                                    implied "PLP" offset

                                0x86 ->
                                    zeroPage "STX" offset memory

                                0x96 ->
                                    zeroPageY "STX" offset memory

                                0x8E ->
                                    absolute "STX" offset memory

                                0x84 ->
                                    zeroPage "STY" offset memory

                                0x94 ->
                                    zeroPageX "STY" offset memory

                                0x8C ->
                                    absolute "STY" offset memory

                                _ ->
                                    Just ( 1, Undefined offset )
                    in
                        case decoded of
                            Nothing ->
                                inputList

                            Just ( bytesRead, instruction ) ->
                                let
                                    nextOffset =
                                        offset + bytesRead

                                    outputList =
                                        instruction :: inputList
                                in
                                    disassemble_ nextOffset (remainingInstructions - 1) memory outputList


implied : String -> Int -> Maybe ( Int, Instruction )
implied mnemonic offset =
    Just ( 1, Known offset mnemonic Implied )


accumulator : String -> Int -> Maybe ( Int, Instruction )
accumulator mnemonic offset =
    Just ( 1, Known offset mnemonic Accumulator )


immediate : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
immediate mnemonic offset memory =
    ByteArray.get (offset + 1) memory
        |> Maybe.map (\val -> ( 2, Known offset mnemonic (Immediate val) ))


zeroPage : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
zeroPage mnemonic offset memory =
    ByteArray.get (offset + 1) memory
        |> Maybe.map (\val -> ( 2, Known offset mnemonic (ZeroPage val) ))


zeroPageX : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
zeroPageX mnemonic offset memory =
    ByteArray.get (offset + 1) memory
        |> Maybe.map (\val -> ( 2, Known offset mnemonic (ZeroPageX val) ))


zeroPageY : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
zeroPageY mnemonic offset memory =
    ByteArray.get (offset + 1) memory
        |> Maybe.map (\val -> ( 2, Known offset mnemonic (ZeroPageY val) ))


absolute : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
absolute mnemonic offset memory =
    ByteArray.get16 (offset + 1) memory
        |> Maybe.map (\val -> ( 3, Known offset mnemonic (Absolute val) ))


indirect : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
indirect mnemonic offset memory =
    ByteArray.get16 (offset + 1) memory
        |> Maybe.map (\val -> ( 3, Known offset mnemonic (Indirect val) ))


relative : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
relative mnemonic offset memory =
    ByteArray.get (offset + 1) memory
        |> Maybe.map (\val -> ( 2, Known offset mnemonic (Relative val) ))


absoluteX : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
absoluteX mnemonic offset memory =
    ByteArray.get16 (offset + 1) memory
        |> Maybe.map (\val -> ( 3, Known offset mnemonic (AbsoluteX val) ))


absoluteY : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
absoluteY mnemonic offset memory =
    ByteArray.get16 (offset + 1) memory
        |> Maybe.map (\val -> ( 3, Known offset mnemonic (AbsoluteY val) ))


indexedIndirect : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
indexedIndirect mnemonic offset memory =
    ByteArray.get (offset + 1) memory
        |> Maybe.map (\val -> ( 2, Known offset mnemonic (IndexedIndirect val) ))


indirectIndexed : String -> Int -> ByteArray -> Maybe ( Int, Instruction )
indirectIndexed mnemonic offset memory =
    ByteArray.get (offset + 1) memory
        |> Maybe.map (\val -> ( 2, Known offset mnemonic (IndirectIndexed val) ))
