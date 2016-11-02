module CssCommon exposing (namespace, helpers, styles, CommonStyles(List))

import Css exposing (..)
import Html.CssHelpers


namespace =
    "rsnes-debugger"


helpers =
    Html.CssHelpers.withNamespace namespace


type CommonStyles
    = List


styles =
    (.) List
        [ listStyleType none
        , padding (px 0)
        ]
