module Styles exposing (css)

import Html.Attributes
import Color
import Css exposing (..)
import Css.Elements exposing (body, ul, li, div)
import Css.Namespace exposing (namespace)
import Registers
import Instructions
import CssCommon


css =
    (stylesheet << namespace CssCommon.namespace)
        [ body
            [ overflowX auto
            , minWidth (px 1280)
            ]
        , CssCommon.styles
        , Registers.styles
        , Instructions.styles
        ]
