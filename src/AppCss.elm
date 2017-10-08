module AppCss exposing (css)

import Breakpoints
import Byte
import Colors
import Console
import Css exposing (..)
import Css.Elements exposing (body, div, html, li, ul)
import Css.Namespace exposing (namespace)
import HexEditor
import Instruction
import Main
import Registers
import Styles


css : Stylesheet
css =
    (stylesheet << namespace "") <|
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
            , Main.styles
            , Instruction.styles
            , Console.styles
            , HexEditor.styles
            , Breakpoints.styles
            , Registers.styles
            ]
