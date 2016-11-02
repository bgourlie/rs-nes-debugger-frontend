module Instructions exposing (view, styles, decodeStartRange, decodeEndRange)

import Html exposing (Html, Attribute)
import List exposing (map, map2)
import ParseInt exposing (toHex)
import Css exposing ((#), (.))
import Css.Elements as CssElem
import CssCommon
import Registers


{ id, class, classList } =
    CssCommon.helpers


view : Int -> List String -> Html msg
view pc decodedRom =
    Html.div [ id Instructions ] [ instructionView pc decodedRom ]


instructionView : Int -> List String -> Html msg
instructionView pc decodedRom =
    Html.ul []
        (map
            (\( str, addr ) ->
                Html.li [ instructionClass addr pc ]
                    [ Html.div [] [ Html.text <| "0x" ++ toHex addr ]
                    , Html.div [] [ Html.text str ]
                    ]
            )
            (map2 (,) decodedRom [decodeStartRange pc..decodeEndRange pc])
        )


type CssIds
    = Instructions
    | CurrentInstruction


styles =
    (#) Instructions
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


instructionClass address pc =
    if address == pc then
        class [ CurrentInstruction ]
    else
        class []


decodeStartRange : Int -> Int
decodeStartRange pc =
    if pc < 20 then
        0
    else
        pc - 20


decodeEndRange : Int -> Int
decodeEndRange pc =
    pc + 20
