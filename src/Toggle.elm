module Toggle exposing (view, styles)

import Html exposing (div, input, label, text, Html)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Css exposing ((.))
import Css.Elements
import CssCommon
import Colors


{ id, class, classList } =
    CssCommon.helpers


view : msg -> String -> String -> Bool -> Html msg
view clickHandler id lbl checked =
    div [ class [ Toggle ] ]
        [ div []
            [ input [ Html.Attributes.id id, type_ "checkbox", Html.Attributes.checked checked ] []
            , label [ Html.Attributes.for id, onClick clickHandler ] []
            ]
        , div [ class [ ToggleLabel ] ]
            [ text lbl
            ]
        ]


styles =
    [ (.) Toggle
        [ Css.display Css.inlineBlock
        , Css.textAlign Css.center
        , Css.children
            [ Css.Elements.div
                [ Css.children
                    [ Css.Elements.input
                        [ Css.display Css.none
                        , Css.adjacentSiblings
                            [ Css.Elements.label
                                [ Css.backgroundColor (Css.hex Colors.toggleOffBackgroundColor)
                                , Css.borderRadius (Css.em 2)
                                , Css.padding (Css.px 2)
                                , Css.property "transition" "all .4s ease"
                                , Css.outline Css.none
                                , Css.display Css.inlineBlock
                                , Css.height (Css.em 2)
                                , Css.width (Css.em 4)
                                , Css.position Css.relative
                                , Css.cursor Css.pointer
                                , Css.property "user-select" "none"
                                , Css.after
                                    [ Css.borderRadius (Css.pct 50)
                                    , Css.backgroundColor (Css.hex Colors.toggleButtonColor)
                                    , Css.left (Css.px 0)
                                    , Css.position Css.relative
                                    , Css.display Css.block
                                    , Css.property "content" "''"
                                    , Css.width (Css.pct 50)
                                    , Css.height (Css.pct 100)
                                    , Css.property "transition" "all .2s ease"
                                    ]
                                ]
                            ]
                        , Css.checked
                            [ Css.adjacentSiblings
                                [ Css.Elements.label
                                    [ Css.backgroundColor (Css.hex Colors.toggleOnBackgroundColor)
                                    , Css.after
                                        [ Css.left (Css.pct 50)
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , (.) ToggleLabel
        [ Css.fontSize (Css.pct 80)
        ]
    ]


type CssClasses
    = Toggle
    | ToggleLabel
