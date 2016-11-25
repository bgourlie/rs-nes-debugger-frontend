module Instruction exposing (view, styles, decoder, Instruction, CssIds(CurrentInstruction))

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import List exposing (map, map2)
import Svg exposing (svg)
import Svg.Attributes
import Json.Decode as Json exposing (Decoder, field)
import ParseInt exposing (toHex)
import Css exposing ((#), (.))
import Css.Elements as CssElem
import CssCommon
import Registers
import Byte
import Breakpoints
import AddressingMode
import Colors


{ id, class, classList } =
    CssCommon.helpers


type alias Instruction =
    { mnemonic : String
    , addressingMode : AddressingMode.AddressingMode
    , offset : Int
    }


type alias Model a =
    { a
        | instructions : List Instruction
        , registers : Registers.Registers
        , breakpoints : Breakpoints.Breakpoints
        , byteDisplay : Byte.Display
    }


decoder : Decoder Instruction
decoder =
    Json.map3 Instruction
        (field "mnemonic" Json.string)
        (field "addressing_mode" AddressingMode.decoder)
        (field "offset" Json.int)


view : (Int -> msg) -> Model a -> Html msg
view breakpointClickHandler model =
    let
        pc =
            model.registers.pc

        instructions =
            model.instructions

        breakpoints =
            model.breakpoints

        byteDisplay =
            model.byteDisplay
    in
        Html.table [ id Instructions ]
            (map
                (\instruction ->
                    Html.tr (currentInstructionAttrs instruction.offset pc)
                        [ Html.td [ class [ Gutter ] ]
                            [ Html.div [ class [ MemoryLocation ] ] [ Byte.view byteDisplay instruction.offset ]
                            , Html.div [ breakpointClass breakpoints instruction.offset, onClick <| breakpointClickHandler instruction.offset ]
                                [ breakpointCircle
                                ]
                            ]
                        , Html.td []
                            [ Html.span [ class [ Mnemonic ] ] [ Html.text instruction.mnemonic ]
                            , Html.span [] [ Html.text " " ]
                            , Html.span [ class [ Operand ] ] [ AddressingMode.view byteDisplay instruction.addressingMode ]
                            ]
                        ]
                )
                instructions
            )


type CssIds
    = Instructions
    | CurrentInstruction


type CssClasses
    = Gutter
    | BreakpointHitBox
    | BreakpointOn
    | MemoryLocation
    | Mnemonic
    | Operand


breakpointCircle : Html.Html msg
breakpointCircle =
    svg
        [ Svg.Attributes.width "1.6ch", Svg.Attributes.height "1.6ch" ]
        [ Svg.circle
            [ Svg.Attributes.fill Colors.breakpointColor
            , Svg.Attributes.cx "0.8ch"
            , Svg.Attributes.cy "0.8ch"
            , Svg.Attributes.r "0.8ch"
            ]
            []
        ]


styles =
    [ (#) Instructions
        [ Css.fontFamilies [ "monospace" ]
        , Css.fontSize (Css.em 1.0)
        , Css.property "border-spacing" "0"
        , Css.height (Css.pct 100)
        , Css.width (Css.pct 100)
        , Css.children
            [ (.) CurrentInstruction
                [ Css.backgroundColor Colors.currentLine
                ]
            ]
        ]
    , (.) Gutter
        [ Css.color Colors.lineNumber
        , Css.backgroundColor Colors.gutterBackground
        , Css.borderRight3 (Css.px 1) Css.solid Colors.gutterBorder
        , Css.paddingRight (Css.em 0.5)
        , Css.width (Css.pct 1)
        , Css.whiteSpace Css.noWrap
        , Css.property "user-select" "none"
        ]
    , (.) MemoryLocation
        [ Css.display Css.inlineBlock
        ]
    , (.) BreakpointHitBox
        [ Css.display Css.inlineBlock
        , Css.paddingLeft (Css.em 0.5)
        , Css.opacity (Css.num 0)
        ]
    , (.) BreakpointOn
        [ Css.opacity (Css.num 1.0)
        ]
    , (.) Mnemonic
        [ Css.color Colors.mnemonic
        , Css.paddingLeft (Css.em 0.5)
        ]
    ]


currentInstructionAttrs address pc =
    if address == pc then
        [ class [ CurrentInstruction ] ]
    else
        []


breakpointClass breakpoints offset =
    if Breakpoints.isSet breakpoints offset then
        class [ BreakpointHitBox, BreakpointOn ]
    else
        class [ BreakpointHitBox ]
