module AppCss exposing (css)

import Css exposing (..)
import Css.Elements exposing (html, body, ul, li, div)
import Css.Namespace exposing (namespace)
import Styles
import Main
import Registers
import Instruction
import Console
import Byte
import Icons
import ToggleButton
import HexEditor
import Icons
import Colors


commonStyles =
    [ (.) Styles.List
        [ Css.listStyleType Css.none
        , Css.padding (Css.px 0)
        , Css.margin (Css.px 0)
        ]
    , (.) Styles.Button
        [ Css.position Css.relative
        , Css.padding Css.zero
        , Css.border3 (Css.px 1) Css.solid Css.transparent
        , Css.borderRadius (Css.px 5)
        , Css.padding (Css.em 0.1)
        , Css.backgroundColor Css.transparent
        , Css.outline Css.none
        , Css.verticalAlign Css.top
        , Css.hover
            [ Css.borderColor (Css.hex Colors.buttonBorderColor)
            ]
        , Css.property "transition" "all .2s ease"
        , Css.cursor Css.pointer
        ]
    ]


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
            , commonStyles
            , Main.styles
            , Registers.styles
            , Instruction.styles
            , Console.styles
            , Byte.styles
            , ToggleButton.styles
            , HexEditor.styles
            , Icons.styles
            ]
