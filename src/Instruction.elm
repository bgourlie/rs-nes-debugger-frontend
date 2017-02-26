module Instruction exposing (view, styles, decoder, request, getOffset, Instruction, OffsetMap)

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Http
import List
import Dict exposing (Dict)
import Json.Decode as Json exposing (Decoder, field)
import Css
import Css.Elements
import ParseInt exposing (toHex)
import Styles
import Registers
import Byte
import Breakpoints
import AddressingMode
import Icons
import Colors
import DebuggerState


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
        , instructionPivot : Int
        , registers : Registers.Registers
        , memory : DebuggerState.Memory
        , breakpoints : Breakpoints.Breakpoints
        , offsetByteFormat : Byte.Format
        , operandByteFormat : Byte.Format
        , memoryByteFormat : Byte.Format
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
            Maybe.withDefault 0 (Dict.get model.instructionPivot model.instructionOffsetMap)

        instructionsToDrop =
            max 0 (pivotIndex - halfWindowSize)
    in
        Html.table [ id Styles.Instructions ]
            (model.instructions
                |> List.drop instructionsToDrop
                |> List.take model.instructionsDisplayed
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
                Html.td [ class [ Styles.InstructionValue ] ]
                    [ Html.span [ class [ Styles.Mnemonic ] ] [ Html.text mnemonic ]
                    , Html.text " "
                    , Html.span [ class [ Styles.Operand ] ] (AddressingMode.view model.operandByteFormat addressingMode)
                    , addressingModeMemoryView model.memoryByteFormat amMemory
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
                    [ Html.span [ class [ Styles.AddressModeMemoryLocation ] ]
                        [ Html.text "Target: "
                        , memoryView byteFormat targetAddr
                        ]
                    , Html.span [ class [ Styles.AddressModeMemoryValue ] ]
                        [ Html.text "Value: "
                        , valueView byteFormat targetValue
                        ]
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
                    "0x" ++ String.padLeft 4 '0' (toHex byte)
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
                , Css.alignItems Css.stretch
                , Styles.withClass Styles.CurrentInstruction
                    [ Css.backgroundColor Colors.currentLine
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
                                [ Css.color Colors.addressModeLiveValue
                                , Css.paddingLeft (Css.em 1)
                                , Css.children
                                    [ Styles.class Styles.AddressModeMemoryValue
                                        [ Css.paddingLeft (Css.em 1)
                                        ]
                                    ]
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
