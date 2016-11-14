module Instruction exposing (view, styles, decoder, Model)

import Html exposing (Html, Attribute)
import Json.Decode as Json exposing (Decoder, (:=))
import List exposing (map, map2)
import ParseInt exposing (toHex)
import Css exposing ((#), (.))
import Css.Elements as CssElem
import CssCommon
import Registers


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
    Html.div [ id Instructions ] [ instructionView pc instructions ]


instructionView : Int -> List Model -> Html msg
instructionView pc instructions =
    Html.ul [ class [ CssCommon.List ] ]
        (map
            (\instruction ->
                Html.li [ instructionClass instruction.offset pc ]
                    [ Html.div [] [ Html.text <| "0x" ++ toHex instruction.offset ]
                    , Html.div [] [ Html.text <| instruction.mnemonic ++ " " ++ instruction.operand ]
                    ]
            )
            instructions
        )


type CssIds
    = Instructions
    | CurrentInstruction


styles =
    [ (#) Instructions
        [ Css.fontFamilies [ "monospace" ]
        , Css.children
            [ CssElem.ul
                [ Css.children
                    [ (.) CurrentInstruction [ Css.backgroundColor (Css.hex "#ccffaa") ]
                    , CssElem.li
                        [ Css.children
                            [ CssElem.div
                                [ Css.display Css.inlineBlock
                                , Css.paddingRight (Css.px 10)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]


instructionClass address pc =
    if address == pc then
        class [ CurrentInstruction ]
    else
        class []
