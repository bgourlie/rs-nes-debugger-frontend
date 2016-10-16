module Styles exposing (css, helpers, CssIds(Instructions, CurrentInstruction))

import Html.Attributes
import Color
import Css exposing (..)
import Css.Elements exposing (body, ul, li, div)
import Css.Namespace exposing (namespace)
import Html.CssHelpers


type CssIds
    = Instructions
    | CurrentInstruction


myNamespace =
    "rsnes-debugger"


css =
    (stylesheet << namespace myNamespace)
        [ body
            [ overflowX auto
            , minWidth (px 1280)
            ]
        , (#) Instructions
            [ fontFamilies [ "monospace" ]
            , children
                [ ul
                    [ listStyleType none
                    , padding (px 0)
                    , children
                        [ (.) CurrentInstruction [ backgroundColor (hex "#ccffaa") ]
                        , li
                            [ children
                                [ div
                                    [ display inlineBlock
                                    , paddingRight (px 10)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


helpers =
    Html.CssHelpers.withNamespace myNamespace
