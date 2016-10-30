module Registers exposing (new, decoder, Model, view)

import Bitwise exposing (and)
import Html exposing (div, ul, li, text, Html)
import Json.Decode as Json exposing (Decoder, (:=))


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
    ul []
        [ li [] [ text <| "Program Counter: " ++ toString model.pc ]
        , li [] [ text <| "Stack Pointer: " ++ toString model.sp ]
        , li [] [ text <| "Accumulator: " ++ toString model.acc ]
        , li [] [ text <| "Index Register (X): " ++ toString model.x ]
        , li [] [ text <| "Index Register (Y): " ++ toString model.y ]
        , li [] [ statusFlagsView model ]
        ]


statusFlagsView : Model -> Html msg
statusFlagsView model =
    div []
        [ div [] [ text "Status Flags" ]
        , ul []
            [ li [] [ text <| "carry: " ++ toString (getCarry model) ]
            , li [] [ text <| "zero: " ++ toString (getZero model) ]
            , li [] [ text <| "interrupt: " ++ toString (getInterrupt model) ]
            , li [] [ text <| "decimal: " ++ toString (getDecimal model) ]
            , li [] [ text <| "overflow: " ++ toString (getOverflow model) ]
            , li [] [ text <| "negative: " ++ toString (getNegative model) ]
            ]
        ]
