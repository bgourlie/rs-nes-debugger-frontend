module Styles exposing (css)

import Css exposing (..)
import Css.Elements exposing (html, body, ul, li, div)
import Css.Namespace exposing (namespace)
import Main
import Registers
import Instruction
import Console
import CssCommon
import Colors


css : Stylesheet
css =
    (stylesheet << namespace CssCommon.namespace) <|
        List.concat
            [ [ html
                    [ boxSizing borderBox
                    ]
              , everything
                    [ boxSizing inherit
                    , after
                        [ boxSizing inherit
                        ]
                    , before
                        [ boxSizing inherit
                        ]
                    ]
              , body
                    [ padding (px 0)
                    , margin (px 0)
                    , backgroundColor Colors.background
                    , color Colors.foreground
                    , fontFamily monospace
                    ]
              ]
            , CssCommon.styles
            , Main.styles
            , Registers.styles
            , Instruction.styles
            , Console.styles
            ]
