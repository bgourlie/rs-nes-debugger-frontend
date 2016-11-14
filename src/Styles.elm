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
                    [ padding (px 0)
                    , margin (px 0)
                    , backgroundColor Colors.background
                    , color Colors.foreground
                    ]
              ]
            , CssCommon.styles
            , Registers.styles
            , Instruction.styles
            , Main.styles
            ]
