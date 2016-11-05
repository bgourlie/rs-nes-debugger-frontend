module CssCommon exposing (namespace, helpers, styles, CommonStyles(List, InlineList))

import Css exposing (..)
import Css.Elements
import Html.CssHelpers


namespace =
    "rsnes-debugger"


helpers =
    Html.CssHelpers.withNamespace namespace


type CommonStyles
    = List
    | InlineList


styles =
    [ (.) List
        [ listStyleType none
        , padding (px 0)
        ]
    , (.) InlineList
        [ listStyleType none
        , padding (px 0)
        , margin (px 0)
        , children
            [ Css.Elements.li
                [ display inlineBlock
                , paddingLeft (em 1)
                , firstChild
                    [ paddingLeft (px 0)
                    ]
                ]
            ]
        ]
    ]
