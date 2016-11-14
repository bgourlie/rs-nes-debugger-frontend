module Instruction exposing (view, styles, decoder, Model)

import Html exposing (Html, Attribute)
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


view : Int -> List Model -> Html msg
view pc instructions =
    Html.table [ id Instructions ]
        (map
            (\instruction ->
                Html.tr [ instructionClass instruction.offset pc ]
                    [ Html.td [ class [ Gutter ] ] [ Html.text <| "0x" ++ toHex instruction.offset ]
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
    | Mnemonic
    | Operand


styles =
    [ (#) Instructions
        [ Css.fontFamilies [ "monospace" ]
        , Css.property "border-spacing" "0"
        , Css.children
            [ (.) CurrentInstruction
                [ Css.backgroundColor Colors.currentLine
                ]
            ]
        ]
    , (.) Gutter
        [ Css.color Colors.lineNumber
        , Css.width (Css.pct 1)
        , Css.backgroundColor Colors.gutterBackground
        , Css.borderRight3 (Css.px 1) Css.solid Colors.gutterBorder
        , Css.paddingRight (Css.em 1)
        , Css.property "user-select" "none"
        ]
    , (.) Mnemonic
        [ Css.color Colors.mnemonic
        , Css.paddingLeft (Css.em 1)
        ]
    ]


instructionClass address pc =
    if address == pc then
        class [ CurrentInstruction ]
    else
        class []
