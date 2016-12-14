module Byte exposing (view8, view16, toggleDisplayView, styles, Format(Hex, Dec))

import Html exposing (Html, Attribute, div, text, input, header, span, label)
import Html.Attributes exposing (disabled, checked, type_, name)
import Html.Events exposing (onClick)
import ParseInt exposing (toHex)
import Css
import Css.Elements
import Styles


{ id, class, classList } =
    Styles.helpers


type alias Byte =
    Int


type Format
    = Hex
    | Dec


type alias Model a =
    { a
        | byteFormat : Format
    }


view8 : Format -> Byte -> Html msg
view8 display byte =
    let
        str =
            case display of
                Hex ->
                    "0x" ++ String.padLeft 2 '0' (toHex byte)

                Dec ->
                    String.padLeft 3 '0' (toString byte)
    in
        span [] [ text str ]


view16 : Format -> Byte -> Html msg
view16 display byte =
    let
        str =
            case display of
                Hex ->
                    "0x" ++ String.padLeft 4 '0' (toHex byte)

                Dec ->
                    String.padLeft 5 '0' (toString byte)
    in
        span [] [ text str ]


toggleDisplayView : (Format -> msg) -> Model a -> Html msg
toggleDisplayView updateDisplayHandler model =
    div [ id Styles.ByteFormatToggle ]
        [ label []
            [ input
                [ name <| toString Styles.ByteFormatToggle
                , type_ "radio"
                , onClick <| updateDisplayHandler Hex
                , checked <| isSelected Hex model
                ]
                []
            , text "Hex"
            ]
        , label []
            [ input
                [ name <| toString Styles.ByteFormatToggle
                , type_ "radio"
                , onClick <| updateDisplayHandler Dec
                , checked <| isSelected Dec model
                ]
                []
            , text "Decimal"
            ]
        ]


isSelected : Format -> Model a -> Bool
isSelected display model =
    display == model.byteFormat


styles : List Css.Snippet
styles =
    [ Styles.id Styles.ByteFormatToggle
        [ Css.verticalAlign Css.top
        , Css.children
            [ Css.Elements.label
                [ Css.marginLeft (Css.px 10)
                ]
            ]
        ]
    ]
