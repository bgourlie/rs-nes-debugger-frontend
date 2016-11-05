module Styles exposing (css)

import Css exposing (..)
import Css.Elements exposing (body, ul, li, div)
import Css.Namespace exposing (namespace)
import Main
import Registers
import Instructions
import CssCommon


css : Stylesheet
css =
    (stylesheet << namespace CssCommon.namespace) <|
        List.concat
            [ [ body
                    [ overflowX auto
                    , minWidth (px 1280)
                    ]
              ]
            , CssCommon.styles
            , Registers.styles
            , Instructions.styles
            , Main.styles
            ]
