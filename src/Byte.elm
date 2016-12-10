module Byte exposing (view, toggleDisplayView, styles, Format(Hex, Dec))

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


type Format
    = Hex
    | Dec


type alias Model a =
    { a
        | byteFormat : Format
    }


view : Format -> Byte -> Html msg
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


toggleDisplayView : (Format -> msg) -> Model a -> Html msg
toggleDisplayView updateDisplayHandler model =
    div [ id ByteFormatToggle ]
        [ label []
            [ input
                [ name <| toString ByteFormatToggle
                , type_ "radio"
                , onClick <| updateDisplayHandler Hex
                , checked <| isSelected Hex model
                ]
                []
            , text "Hex"
            ]
        , label []
            [ input
                [ name <| toString ByteFormatToggle
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
    [ (#) ByteFormatToggle
        [ Css.verticalAlign Css.top
        , Css.children
            [ Css.Elements.label
                [ Css.marginLeft (Css.px 10)
                ]
            ]
        ]
    ]


type CssIds
    = ByteFormatToggle
