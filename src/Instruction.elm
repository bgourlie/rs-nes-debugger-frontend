module Instruction exposing (view, styles, decoder, request, getOffset, Instruction, OffsetMap)

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Http
import List
import Dict exposing (Dict)
import Json.Decode as Json exposing (Decoder, field)
import Css
import Styles
import Registers
import Byte
import Breakpoints
import AddressingMode
import MemorySnapshot
import Icons
import Colors


{ id, class, classList } =
    Styles.helpers


type Instruction
    = Known Int String AddressingMode.AddressingMode
    | Undefined Int


type alias OffsetMap =
    Dict Int Int


type alias Model a =
    { a
        | instructions : List Instruction
        , instructionsDisplayed : Int
        , instructionOffsetMap : OffsetMap
        , registers : Registers.Registers
        , memory : MemorySnapshot.MemorySnapshot
        , breakpoints : Breakpoints.Breakpoints
        , byteFormat : Byte.Format
    }


decoder : Decoder (List Instruction)
decoder =
    Json.list instructionDecoder


instructionDecoder : Decoder Instruction
instructionDecoder =
    (field "kind" Json.string)
        |> Json.andThen
            (\kind ->
                case kind of
                    "Known" ->
                        knownDecoder

                    "Undefined" ->
                        undefinedDecoder

                    _ ->
                        Json.fail "Unexpected instruction kind"
            )


knownDecoder : Decoder Instruction
knownDecoder =
    Json.map3 (,,)
        (field "offset" Json.int)
        (field "mnemonic" Json.string)
        (field "am" AddressingMode.decoder)
        |> Json.andThen (\( offset, mnemonic, am ) -> Json.succeed (Known offset mnemonic am))


undefinedDecoder : Decoder Instruction
undefinedDecoder =
    (field "offset" Json.int)
        |> Json.andThen (\offset -> Json.succeed (Undefined offset))


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
        halfWindowSize =
            floor <| (toFloat model.instructionsDisplayed) / 2.0

        pivotIndex =
            Maybe.withDefault 0 (Dict.get model.registers.pc model.instructionOffsetMap)

        instructionsToDrop =
            max 0 (pivotIndex - halfWindowSize)
    in
        Html.table [ class [ Styles.Instructions ] ]
            (model.instructions
                |> List.drop instructionsToDrop
                |> List.take model.instructionsDisplayed
                |> List.map
                    (\instruction ->
                        let
                            offset =
                                getOffset instruction
                        in
                            Html.tr ([ classList [ ( Styles.CurrentInstruction, offset == model.registers.pc ) ] ])
                                [ Html.td [ class [ Styles.Gutter ] ]
                                    [ Html.div [ class [ Styles.MemoryLocation ] ] [ Byte.view16 model.byteFormat offset ]
                                    , Html.div [ breakpointClasses model.breakpoints offset, onClick <| breakpointClickHandler offset ]
                                        [ Icons.breakpoint
                                        ]
                                    ]
                                , instructionCell model instruction
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


instructionCell : Model a -> Instruction -> Html msg
instructionCell model instr =
    case instr of
        Known offset mnemonic addressingMode ->
            let
                amMemory =
                    if offset == model.registers.pc then
                        AddressingMode.getMemory model.memory model.registers addressingMode
                    else
                        Nothing
            in
                Html.td []
                    [ Html.span [ class [ Styles.Mnemonic ] ] [ Html.text mnemonic ]
                    , Html.span [] [ Html.text " " ]
                    , Html.span [ class [ Styles.Operand ] ] [ AddressingMode.view model.byteFormat addressingMode ]
                    , addressingModeMemoryView model.byteFormat amMemory
                    ]

        Undefined offset ->
            Html.td [] [ Html.span [ class [ Styles.UndefinedOpcode ] ] [ Html.text "--" ] ]


addressingModeMemoryView : Byte.Format -> Maybe ( Int, Int ) -> Html msg
addressingModeMemoryView byteFormat amMemory =
    case amMemory of
        Just mem ->
            let
                ( targetAddr, targetValue ) =
                    mem
            in
                Html.span [ class [ Styles.AddressModeValues ] ]
                    [ Html.span [ class [ Styles.AddressModeMemoryLocation ] ]
                        [ Html.text "Target: "
                        , Byte.view16 byteFormat targetAddr
                        ]
                    , Html.span [ class [ Styles.AddressModeMemoryValue ] ]
                        [ Html.text "Value: "
                        , Byte.view8 byteFormat targetValue
                        ]
                    ]

        Nothing ->
            Html.span [] []


styles : List Css.Snippet
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
    , Styles.class Styles.AddressModeValues
        [ Css.color Colors.addressModeLiveValue
        , Css.paddingLeft (Css.em 1)
        ]
    , Styles.class Styles.AddressModeMemoryValue
        [ Css.paddingLeft (Css.em 1)
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
    , Styles.class Styles.UndefinedOpcode
        [ Css.color Colors.undefinedOpcode
        , Css.paddingLeft (Css.em 0.5)
        ]
    ]


breakpointClasses : Breakpoints.Breakpoints -> Int -> Attribute msg
breakpointClasses breakpoints offset =
    classList [ ( Styles.BreakpointHitBox, True ), ( Styles.BreakpointOn, Breakpoints.isSet breakpoints offset ) ]
