module AppCss exposing (css)

import Css exposing (..)
import Css.Elements exposing (html, body, ul, li, div)
import Css.Namespace exposing (namespace)
import Styles
import Main
import Instruction
import Console
import Byte
import HexEditor
import Colors
import Registers
import Breakpoints


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
