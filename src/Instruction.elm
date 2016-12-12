module Instruction exposing (view, styles, decoder, request, Instruction, OffsetMap, CssIds(CurrentInstruction))

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Http
import List exposing (map, map2)
import Dict exposing (Dict)
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
import Icons
import Colors


{ id, class, classList } =
    CssCommon.helpers


type alias Instruction =
    { mnemonic : String
    , addressingMode : AddressingMode.AddressingMode
    , offset : Int
    }


type alias OffsetMap =
    Dict Int Int


type alias Model a =
    { a
        | instructions : List Instruction
        , instructionsDisplayed : Int
        , instructionOffsetMap : OffsetMap
        , registers : Registers.Registers
        , breakpoints : Breakpoints.Breakpoints
        , byteFormat : Byte.Format
    }


decoder : Decoder (List Instruction)
decoder =
    Json.list <|
        Json.map3 Instruction
            (field "mnemonic" Json.string)
            (field "addressing_mode" AddressingMode.decoder)
            (field "offset" Json.int)


endpoint : String
endpoint =
    "http://localhost:9975/instructions"


request : (Http.Error -> msg) -> (List Instruction -> msg) -> Cmd msg
request failHandler successHandler =
    let
        result =
            (\r ->
                case r of
                    Ok r ->
                        successHandler r

                    Err e ->
                        failHandler e
            )
    in
        Http.send result (Http.get endpoint decoder)


view : (Int -> msg) -> Model a -> Html msg
view breakpointClickHandler model =
    let
        pc =
            model.registers.pc

        instructions =
            model.instructions

        instructionOffsetMap =
            model.instructionOffsetMap

        breakpoints =
            model.breakpoints

        byteFormat =
            model.byteFormat

        instructionsDisplayed =
            model.instructionsDisplayed

        halfWindowSize =
            floor <| (toFloat instructionsDisplayed) / 2.0

        pivotIndex =
            Maybe.withDefault 0 (Dict.get pc instructionOffsetMap)

        instructionsToDrop =
            max 0 (pivotIndex - halfWindowSize)
    in
        Html.table [ id Instructions ]
            (instructions
                |> List.drop instructionsToDrop
                |> List.take instructionsDisplayed
                |> map
                    (\instruction ->
                        Html.tr (currentInstructionAttrs instruction.offset pc)
                            [ Html.td [ class [ Gutter ] ]
                                [ Html.div [ class [ MemoryLocation ] ] [ Byte.view16 byteFormat instruction.offset ]
                                , Html.div [ breakpointClass breakpoints instruction.offset, onClick <| breakpointClickHandler instruction.offset ]
                                    [ Icons.breakpoint
                                    ]
                                ]
                            , Html.td []
                                [ Html.span [ class [ Mnemonic ] ] [ Html.text instruction.mnemonic ]
                                , Html.span [] [ Html.text " " ]
                                , Html.span [ class [ Operand ] ] [ AddressingMode.view byteFormat instruction.addressingMode ]
                                ]
                            ]
                    )
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


styles =
    [ (#) Instructions
        [ Css.fontFamilies [ "monospace" ]
        , Css.fontSize (Css.em 1.0)
        , Css.property "border-spacing" "0"
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
        , Css.property "transition" "opacity .15s"
        , Css.paddingLeft (Css.em 0.6)
        , Css.opacity (Css.num 0)
        , Css.cursor Css.pointer
        , Css.hover
            [ Css.opacity (Css.num 0.2)
            ]
        ]
    , (.) BreakpointOn
        [ Css.opacity (Css.num 1.0)
        , Css.hover
            [ Css.opacity (Css.num 1.0)
            ]
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
