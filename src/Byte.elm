module Byte exposing (view, toggleDisplayView, styles, Display(Hex, Dec))

import Html exposing (Html, Attribute, div, text, input, header, span, label)
import Html.Attributes exposing (disabled, checked, type_, name)
import Html.Events exposing (onClick)
import ParseInt exposing (toHex)
import Css exposing ((#))
import Css.Elements
import CssCommon


{ id, class, classList } =
    CssCommon.helpers


type alias Byte =
    Int


type Display
    = Hex
    | Dec


type alias Model a =
    { a
        | byteDisplay : Display
    }


view : Display -> Byte -> Html msg
view display byte =
    let
        str =
            case display of
                Hex ->
                    "0x" ++ toHex byte

                Dec ->
                    toString byte
    in
        span [] [ text str ]


toggleDisplayView : (Display -> msg) -> Model a -> Html msg
toggleDisplayView updateDisplayHandler model =
    div [ id ByteDisplayToggle ]
        [ label []
            [ input
                [ name <| toString ByteDisplayToggle
                , type_ "radio"
                , onClick <| updateDisplayHandler Hex
                , checked <| isSelected Hex model
                ]
                []
            , text "Hex"
            ]
        , label []
            [ input
                [ name <| toString ByteDisplayToggle
                , type_ "radio"
                , onClick <| updateDisplayHandler Dec
                , checked <| isSelected Dec model
                ]
                []
            , text "Decimal"
            ]
        ]


isSelected : Display -> Model a -> Bool
isSelected display model =
    display == model.byteDisplay


styles : List Css.Snippet
styles =
    [ (#) ByteDisplayToggle
        [ Css.children
            [ Css.Elements.label
                [ Css.display Css.block
                ]
            ]
        ]
    ]


type CssIds
    = ByteDisplayToggle