module Styles exposing (css)

import Css exposing (..)
import Css.Elements exposing (body, ul, li, div)
import Css.Namespace exposing (namespace)
import Main
import Registers
import Instruction
import CssCommon
import Colors


css : Stylesheet
css =
    (stylesheet << namespace CssCommon.namespace) <|
        List.concat
            [ [ body
                    [ backgroundColor Colors.background
                    , color Colors.foreground
                    , overflowX auto
                    , minWidth (px 1280)
                    ]
              ]
            , CssCommon.styles
            , Registers.styles
            , Instruction.styles
            , Main.styles
            ]
