module Instruction exposing (view, styles, decoder, request, Instruction, OffsetMap)

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Http
import List
import Dict exposing (Dict)
import Svg exposing (svg)
import Svg.Attributes
import Json.Decode as Json exposing (Decoder, field)
import ParseInt exposing (toHex)
import Css
import Css.Elements as CssElem
import Styles
import Registers
import Byte
import Breakpoints
import AddressingMode
import Icons
import Colors


{ id, class, classList } =
    Styles.helpers


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
        Html.table [ class [ Styles.Instructions ] ]
            (instructions
                |> List.drop instructionsToDrop
                |> List.take instructionsDisplayed
                |> List.map
                    (\instruction ->
                        Html.tr ([ classList [ ( Styles.CurrentInstruction, instruction.offset == pc ) ] ])
                            [ Html.td [ class [ Styles.Gutter ] ]
                                [ Html.div [ class [ Styles.MemoryLocation ] ] [ Byte.view16 byteFormat instruction.offset ]
                                , Html.div [ breakpointClasses breakpoints instruction.offset, onClick <| breakpointClickHandler instruction.offset ]
                                    [ Icons.breakpoint
                                    ]
                                ]
                            , Html.td []
                                [ Html.span [ class [ Styles.Mnemonic ] ] [ Html.text instruction.mnemonic ]
                                , Html.span [] [ Html.text " " ]
                                , Html.span [ class [ Styles.Operand ] ] [ AddressingMode.view byteFormat instruction.addressingMode ]
                                ]
                            ]
                    )
            )


styles =
    [ Styles.class Styles.Instructions
        [ Css.fontFamilies [ "monospace" ]
        , Css.fontSize (Css.em 1.0)
        , Css.property "border-spacing" "0"
        , Css.height (Css.pct 100)
        , Css.width (Css.pct 100)
        , Css.children
            [ Styles.class Styles.CurrentInstruction
                [ Css.backgroundColor Colors.currentLine
                ]
            ]
        ]
    , Styles.class Styles.Gutter
        [ Css.color Colors.lineNumber
        , Css.backgroundColor Colors.gutterBackground
        , Css.borderRight3 (Css.px 1) Css.solid Colors.gutterBorder
        , Css.paddingRight (Css.em 0.5)
        , Css.width (Css.pct 1)
        , Css.whiteSpace Css.noWrap
        , Css.property "user-select" "none"
        ]
    , Styles.class Styles.MemoryLocation
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
        ]
    , Styles.class Styles.BreakpointOn
        [ Css.opacity (Css.num 1.0)
        , Css.hover
            [ Css.opacity (Css.num 1.0)
            ]
        ]
    , Styles.class Styles.Mnemonic
        [ Css.color Colors.mnemonic
        , Css.paddingLeft (Css.em 0.5)
        ]
    ]


breakpointClasses breakpoints offset =
    classList [ ( Styles.BreakpointHitBox, True ), ( Styles.BreakpointOn, Breakpoints.isSet breakpoints offset ) ]
