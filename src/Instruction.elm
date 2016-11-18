module Instruction exposing (view, styles, decoder, Model)

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Svg exposing (svg)
import Svg.Attributes
import Json.Decode as Json exposing (Decoder, (:=))
import List exposing (map, map2)
import ParseInt exposing (toHex)
import Css exposing ((#), (.))
import Css.Elements as CssElem
import CssCommon
import Registers
import Colors


{ id, class, classList } =
    CssCommon.helpers



--


type alias Model =
    { mnemonic : String
    , operand : String
    , offset : Int
    }


decoder : Decoder Model
decoder =
    Json.object3 Model
        ("mnemonic" := Json.string)
        ("operand" := Json.string)
        ("offset" := Json.int)


view : (Int -> msg) -> Set Int -> Int -> List Model -> Html msg
view breakpointClickHandler breakpoints pc instructions =
    Html.table [ id Instructions ]
        (map
            (\instruction ->
                Html.tr [ instructionClass instruction.offset pc ]
                    [ Html.td [ class [ Gutter ] ]
                        [ Html.div [ class [ MemoryLocation ] ] [ Html.text <| "0x" ++ toHex instruction.offset ]
                        , Html.div [ breakpointClass breakpoints instruction.offset, onClick <| breakpointClickHandler instruction.offset ]
                            [ breakpointCircle
                            ]
                        ]
                    , Html.td []
                        [ Html.span [ class [ Mnemonic ] ] [ Html.text instruction.mnemonic ]
                        , Html.span [] [ Html.text " " ]
                        , Html.span [ class [ Operand ] ] [ Html.text instruction.operand ]
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
        [ Svg.Attributes.width "1.2em", Svg.Attributes.height "1.2em" ]
        [ Svg.circle
            [ Svg.Attributes.fill "#FF6666"
            , Svg.Attributes.cx "0.6em"
            , Svg.Attributes.cy "0.6em"
            , Svg.Attributes.r "0.6em"
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


instructionClass address pc =
    if address == pc then
        class [ CurrentInstruction ]
    else
        class []


breakpointClass breakpoints offset =
    if Set.member offset breakpoints then
        class [ BreakpointHitBox, BreakpointOn ]
    else
        class [ BreakpointHitBox ]
