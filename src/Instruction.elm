module Instruction exposing (view, styles)

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import List
import Css
import ParseInt exposing (toHex)
import Styles
import Registers
import Byte
import Breakpoints
import AddressingMode
import Colors
import Memory
import ByteArray exposing (ByteArray)
import Disassembler exposing (Instruction(..))


{ id, class, classList } =
    Styles.helpers


type alias Model a =
    { a
        | instructionsDisplayed : Int
        , disassembleOffset : Int
        , registers : Registers.Registers
        , memory : Memory.Memory
        , breakpoints : Breakpoints.Breakpoints
        , offsetByteFormat : Byte.Format
        , operandByteFormat : Byte.Format
        , memoryByteFormat : Byte.Format
    }


view : (Int -> msg) -> Model a -> Html msg
view breakpointClickHandler model =
    let
        ( _, memory ) =
            model.memory
    in
        Html.table [ id Styles.Instructions ]
            (memory
                |> Disassembler.disassemble model.disassembleOffset model.instructionsDisplayed
                |> List.map
                    (\instruction ->
                        let
                            offset =
                                getOffset instruction
                        in
                            Html.tr
                                [ classList
                                    [ ( Styles.Instruction, True )
                                    , ( Styles.CurrentInstruction, offset == model.registers.pc )
                                    ]
                                ]
                                [ Html.td [ class [ Styles.InstructionGutter ] ]
                                    [ Html.div [ class [ Styles.MemoryLocation ] ] [ memoryView model.offsetByteFormat offset ]
                                    , Html.div
                                        [ classList
                                            [ ( Styles.BreakpointHitBox, True )
                                            , ( Styles.BreakpointOn, Breakpoints.isSet model.breakpoints offset )
                                            ]
                                        , onClick (breakpointClickHandler offset)
                                        ]
                                        [ Breakpoints.icon
                                        ]
                                    ]
                                , instructionCell model memory instruction
                                ]
                    )
            )


getOffset : Instruction -> Int
getOffset instr =
    case instr of
        Known offset _ _ ->
            offset

        Undefined offset ->
            offset


instructionCell : Model a -> ByteArray.ByteArray -> Instruction -> Html msg
instructionCell { registers, memoryByteFormat, operandByteFormat } memory instr =
    case instr of
        Known offset mnemonic addressingMode ->
            let
                amMemory =
                    AddressingMode.getTargetOffset memory registers addressingMode
            in
                Html.td [ class [ Styles.InstructionValue ] ]
                    [ Html.span [ class [ Styles.Mnemonic ] ] [ Html.text mnemonic ]
                    , Html.text " "
                    , Html.span [ class [ Styles.Operand ] ] (AddressingMode.view operandByteFormat addressingMode)
                    , addressingModeMemoryView memoryByteFormat amMemory
                    ]

        Undefined offset ->
            Html.td [ class [ Styles.InstructionValue ] ] [ Html.span [ class [ Styles.UndefinedOpcode ] ] [ Html.text "---" ] ]


addressingModeMemoryView : Byte.Format -> Maybe ( Int, Int ) -> Html msg
addressingModeMemoryView byteFormat amMemory =
    case amMemory of
        Just mem ->
            let
                ( targetAddr, targetValue ) =
                    mem
            in
                Html.span [ class [ Styles.AddressModeValues ] ]
                    [ Html.text " @ "
                    , Html.span [ class [ Styles.AddressModeMemoryLocation ] ] [ memoryView byteFormat targetAddr ]
                    , Html.text " = "
                    , Html.span [ class [ Styles.AddressModeMemoryValue ] ] [ valueView byteFormat targetValue ]
                    ]

        Nothing ->
            Html.text ""


memoryView : Byte.Format -> Int -> Html msg
memoryView display byte =
    let
        str =
            case display of
                Byte.Dec ->
                    String.padLeft 5 '0' (toString byte)

                _ ->
                    -- Default to hex display
                    "0x" ++ String.padLeft 4 '0' (toHex byte)
    in
        Html.text str


valueView : Byte.Format -> Int -> Html msg
valueView display byte =
    let
        str =
            case display of
                Byte.Dec ->
                    String.padLeft 3 '0' (toString byte)

                _ ->
                    -- Default to hex
                    "0x" ++ String.padLeft 2 '0' (toHex byte)
    in
        Html.span [] [ Html.text str ]


styles : List Css.Snippet
styles =
    [ Styles.id Styles.Instructions
        [ Css.width (Css.pct 100)
        , Css.property "border-spacing" "0"
        , Css.children
            [ Styles.class Styles.Instruction
                [ Css.displayFlex
                , Styles.withClass Styles.CurrentInstruction
                    [ Css.backgroundColor Colors.currentLine
                    , Css.children
                        [ Styles.class Styles.InstructionValue
                            [ Css.children
                                [ Styles.class Styles.AddressModeValues
                                    [ Css.color Colors.addressModeActiveValue
                                    ]
                                ]
                            ]
                        ]
                    ]
                , Css.children
                    [ Styles.class Styles.InstructionGutter
                        [ Css.color Colors.lineNumber
                        , Css.backgroundColor Colors.gutterBackground
                        , Css.borderRight3 (Css.px 1) Css.solid Colors.gutterBorder
                        , Css.paddingRight (Css.em 0.5)
                        , Css.whiteSpace Css.noWrap
                        , Css.property "user-select" "none"
                        , Css.children
                            [ Styles.class Styles.MemoryLocation
                                [ Css.display Css.inlineBlock
                                ]
                            , Styles.class Styles.BreakpointHitBox
                                [ Css.display Css.inlineBlock
                                , Css.property "transition" "opacity .15s"
                                , Css.paddingLeft (Css.em 0.6)
                                , Css.opacity (Css.num 0)
                                , Css.cursor Css.pointer
                                , Css.hover
                                    [ Css.opacity (Css.num 0.2)
                                    ]
                                , Styles.withClass Styles.BreakpointOn
                                    [ Css.opacity (Css.num 1.0)
                                    , Css.hover
                                        [ Css.opacity (Css.num 1.0)
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , Styles.class Styles.InstructionValue
                        [ Css.children
                            [ Styles.class Styles.Mnemonic
                                [ Css.color Colors.mnemonic
                                , Css.paddingLeft (Css.em 0.5)
                                ]
                            , Styles.class Styles.UndefinedOpcode
                                [ Css.color Colors.undefinedOpcode
                                , Css.paddingLeft (Css.em 0.5)
                                ]
                            , Styles.class Styles.AddressModeValues
                                [ Css.color Colors.addressModeInactiveValue
                                ]
                            ]
                        , Css.lastChild
                            [ Css.flexGrow (Css.num 1)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
