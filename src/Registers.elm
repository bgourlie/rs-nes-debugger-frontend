module Registers exposing (new, decoder, Model, view, styles)

import Html exposing (div, ul, li, h4, text, table, tr, td, th, Html)
import Html.Attributes exposing (title)
import Bitwise exposing (and)
import Json.Decode as Json exposing (Decoder, (:=))
import Css exposing ((#))
import Css.Elements
import CssCommon


{ id, class, classList } =
    CssCommon.helpers



--


getCarry : Model -> Bool
getCarry model =
    model.stat `and` 0x01 > 0


getZero : Model -> Bool
getZero model =
    model.stat `and` 0x02 > 0


getInterrupt : Model -> Bool
getInterrupt model =
    model.stat `and` 0x04 > 0


getDecimal : Model -> Bool
getDecimal model =
    model.stat `and` 0x08 > 0


getOverflow : Model -> Bool
getOverflow model =
    model.stat `and` 0x40 > 0


getNegative : Model -> Bool
getNegative model =
    model.stat `and` 0x80 > 0


type alias Model =
    { acc : Int
    , x : Int
    , y : Int
    , pc : Int
    , sp : Int
    , stat : Int
    }


new : Model
new =
    { acc = 0
    , x = 0
    , y = 0
    , pc = 0
    , sp = 0
    , stat = 0
    }


decoder : Decoder Model
decoder =
    Json.object6 Model
        ("acc" := Json.int)
        ("x" := Json.int)
        ("y" := Json.int)
        ("pc" := Json.int)
        ("sp" := Json.int)
        ("status" := Json.int)


view : Model -> Html msg
view model =
    div [ id Registers ]
        [ table []
            [ tr []
                [ th [ title "Program Counter" ] [ text "PC" ]
                , th [ title "Stack Pointer" ] [ text "SP" ]
                , th [ title "Accumulator" ] [ text "ACC" ]
                , th [ title "Index (X)" ] [ text "X" ]
                , th [ title "Index (Y)" ] [ text "Y" ]
                ]
            , tr []
                [ td [] [ text <| toString model.pc ]
                , td [] [ text <| toString model.sp ]
                , td [] [ text <| toString model.acc ]
                , td [] [ text <| toString model.x ]
                , td [] [ text <| toString model.y ]
                ]
            ]
        , table []
            [ tr []
                [ th [ title "Carry Flag" ] [ text "C" ]
                , th [ title "Zero Flag" ] [ text "Z" ]
                , th [ title "Interrupt flag" ] [ text "I" ]
                , th [ title "Decimal flag" ] [ text "D" ]
                , th [ title "Overflow flag" ] [ text "V " ]
                , th [ title "Sign flag" ] [ text "S" ]
                ]
            , tr []
                [ td [] [ text <| flagDisplay (getCarry model) ]
                , td [] [ text <| flagDisplay (getZero model) ]
                , td [] [ text <| flagDisplay (getInterrupt model) ]
                , td [] [ text <| flagDisplay (getDecimal model) ]
                , td [] [ text <| flagDisplay (getOverflow model) ]
                , td [] [ text <| flagDisplay (getNegative model) ]
                ]
            ]
        ]


type CssIds
    = Registers


flagDisplay : Bool -> String
flagDisplay val =
    if val then
        toString 1
    else
        toString 0


styles =
    [ (#) Registers
        [ Css.fontFamilies [ "monospace" ]
        , Css.children
            [ Css.Elements.table
                [ Css.display Css.inlineBlock
                ]
            ]
        ]
    ]
