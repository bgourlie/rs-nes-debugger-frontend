module Registers exposing (new, decoder, Registers, view, styles)

import Html exposing (div, ul, li, h4, text, table, tr, td, th, Html)
import Html.Attributes exposing (title, colspan)
import Bitwise exposing (and)
import Json.Decode as Json exposing (Decoder, field)
import Css exposing ((#))
import Css.Elements
import CssCommon
import Byte


{ id, class, classList } =
    CssCommon.helpers


getCarry : Registers -> Bool
getCarry model =
    and model.stat 0x01 > 0


getZero : Registers -> Bool
getZero model =
    and model.stat 0x02 > 0


getInterrupt : Registers -> Bool
getInterrupt model =
    and model.stat 0x04 > 0


getDecimal : Registers -> Bool
getDecimal model =
    and model.stat 0x08 > 0


getOverflow : Registers -> Bool
getOverflow model =
    and model.stat 0x40 > 0


getNegative : Registers -> Bool
getNegative model =
    and model.stat 0x80 > 0


type alias Registers =
    { acc : Int
    , x : Int
    , y : Int
    , pc : Int
    , sp : Int
    , stat : Int
    }


type alias Model a =
    { a
        | registers : Registers
        , cycles : Int
        , byteFormat : Byte.Format
    }


new : Registers
new =
    { acc = 0
    , x = 0
    , y = 0
    , pc = 0
    , sp = 0
    , stat = 0
    }


decoder : Decoder Registers
decoder =
    Json.map6 Registers
        (field "acc" Json.int)
        (field "x" Json.int)
        (field "y" Json.int)
        (field "pc" Json.int)
        (field "sp" Json.int)
        (field "status" Json.int)


view : Model a -> Html msg
view model =
    let
        registers =
            model.registers

        display =
            model.byteFormat

        cycles =
            model.cycles
    in
        table [ id RegistersTable ]
            [ tr []
                [ th [ title "Program Counter" ] [ text "PC" ]
                , th [ title "Stack Pointer" ] [ text "SP" ]
                , th [ title "Accumulator" ] [ text "ACC" ]
                , th [ title "Index (X)" ] [ text "X" ]
                , th [ title "Index (Y)" ] [ text "Y" ]
                , th [ colspan 6 ] [ text "Status" ]
                , th [] [ text "Cycles" ]
                ]
            , tr []
                [ td [] [ Byte.view display registers.pc ]
                , td [] [ Byte.view display registers.sp ]
                , td [] [ Byte.view display registers.acc ]
                , td [] [ Byte.view display registers.x ]
                , td [] [ Byte.view display registers.y ]
                , td [ title "Carry Flag" ] [ text <| "C" ++ flagDisplay (getCarry registers) ]
                , td [ title "Zero Flag" ] [ text <| "Z" ++ flagDisplay (getZero registers) ]
                , td [ title "Interrupt Flag" ] [ text <| "I" ++ flagDisplay (getInterrupt registers) ]
                , td [ title "Decimal Flag" ] [ text <| "D" ++ flagDisplay (getDecimal registers) ]
                , td [ title "Overflow Flag" ] [ text <| "V" ++ flagDisplay (getOverflow registers) ]
                , td [ title "Sign Flag" ] [ text <| "S" ++ flagDisplay (getNegative registers) ]
                , td [] [ text <| toString cycles ]
                ]
            ]


type CssIds
    = RegistersTable
    | Cycles


flagDisplay : Bool -> String
flagDisplay val =
    if val then
        toString 1
    else
        toString 0


styles =
    [ (#) RegistersTable
        [ Css.descendants
            [ Css.Elements.th
                [ Css.width (Css.ch 6)
                ]
            , Css.Elements.td
                [ Css.textAlign Css.center
                ]
            ]
        ]
    ]
