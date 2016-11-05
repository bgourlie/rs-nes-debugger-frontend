module Registers exposing (new, decoder, Model, view, styles)

import Html exposing (div, ul, li, h4, text, Html)
import Html.Attributes exposing (title)
import Bitwise exposing (and)
import Json.Decode as Json exposing (Decoder, (:=))
import Css exposing ((#))
import Css.Elements
import CssCommon


{ id, class, classList } =
    CssCommon.helpers


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
        ("stat" := Json.int)


view : Model -> Html msg
view model =
    div [ id Registers ]
        [ h4 [] [ text "Registers" ]
        , ul [ class [ CssCommon.InlineList ] ]
            [ li [ title "Program Counter" ] [ text <| "PC: " ++ toString model.pc ]
            , li [ title "Stack Pointer" ] [ text <| "SP: " ++ toString model.sp ]
            , li [ title "Accumulator" ] [ text <| "ACC: " ++ toString model.acc ]
            , li [ title "Index (X)" ] [ text <| "X: " ++ toString model.x ]
            , li [ title "Index (Y)" ] [ text <| "Y: " ++ toString model.y ]
            ]
        , div [ id StatusFlags ]
            [ h4 [] [ text "Status Flags" ]
            , ul [ class [ CssCommon.InlineList ] ]
                [ li [] [ text <| "carry: " ++ flagDisplay (getCarry model) ]
                , li [] [ text <| "zero: " ++ flagDisplay (getZero model) ]
                , li [] [ text <| "interrupt: " ++ flagDisplay (getInterrupt model) ]
                , li [] [ text <| "decimal: " ++ flagDisplay (getDecimal model) ]
                , li [] [ text <| "overflow: " ++ flagDisplay (getOverflow model) ]
                , li [] [ text <| "negative: " ++ flagDisplay (getNegative model) ]
                ]
            ]
        ]


type CssIds
    = Registers
    | StatusFlags


flagDisplay : Bool -> String
flagDisplay val =
    if val then
        toString 1
    else
        toString 0


styles =
    [ (#) Registers
        [ Css.fontFamilies [ "monospace" ]
        , Css.descendants
            [ Css.Elements.h4
                [ Css.marginBottom (Css.px 3)
                ]
            ]
        ]
    , (#) StatusFlags
        [ Css.fontFamilies [ "monospace" ]
        ]
    ]
