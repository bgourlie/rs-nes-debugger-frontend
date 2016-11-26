module CssCommon exposing (namespace, helpers, styles, CommonStyles(List, InlineList, Button))

import Css exposing (..)
import Css.Elements
import Html.CssHelpers
import Colors


namespace =
    ""


helpers =
    Html.CssHelpers.withNamespace namespace


type CommonStyles
    = List
    | InlineList
    | Button


styles =
    [ (.) List
        [ listStyleType none
        , padding (px 0)
        , margin (px 0)
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
    , (.) Button
        [ Css.position Css.relative
        , Css.padding Css.zero
        , Css.border3 (Css.px 1) Css.solid Css.transparent
        , Css.borderRadius (Css.px 5)
        , Css.padding2 (Css.em 0.15) (Css.em 0.25)
        , Css.backgroundColor Css.transparent
        , Css.outline Css.none
        , Css.hover
            [ Css.borderColor (Css.hex Colors.buttonBorderColor)
            ]
        , Css.property "transition" "all .2s ease"
        , Css.cursor Css.pointer
        ]
    ]
