module Registers exposing (new, decoder, Registers, view, styles)

import Html exposing (div, ul, li, h4, text, table, tr, td, th, Html)
import Html.Attributes exposing (title)
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
        , byteDisplay : Byte.Display
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
            model.byteDisplay

        cycles =
            model.cycles
    in
        div [ id RegistersElement ]
            [ table [ id RegistersTable ]
                [ tr []
                    [ th [ title "Program Counter" ] [ text "PC" ]
                    , th [ title "Stack Pointer" ] [ text "SP" ]
                    , th [ title "Accumulator" ] [ text "ACC" ]
                    , th [ title "Index (X)" ] [ text "X" ]
                    , th [ title "Index (Y)" ] [ text "Y" ]
                    ]
                , tr []
                    [ td [] [ Byte.view display registers.pc ]
                    , td [] [ Byte.view display registers.sp ]
                    , td [] [ Byte.view display registers.acc ]
                    , td [] [ Byte.view display registers.x ]
                    , td [] [ Byte.view display registers.y ]
                    ]
                ]
            , table [ id StatusTable ]
                [ tr []
                    [ th [ title "Carry Flag" ] [ text "C" ]
                    , th [ title "Zero Flag" ] [ text "Z" ]
                    , th [ title "Interrupt flag" ] [ text "I" ]
                    , th [ title "Decimal flag" ] [ text "D" ]
                    , th [ title "Overflow flag" ] [ text "V " ]
                    , th [ title "Sign flag" ] [ text "S" ]
                    ]
                , tr []
                    [ td [] [ text <| flagDisplay (getCarry registers) ]
                    , td [] [ text <| flagDisplay (getZero registers) ]
                    , td [] [ text <| flagDisplay (getInterrupt registers) ]
                    , td [] [ text <| flagDisplay (getDecimal registers) ]
                    , td [] [ text <| flagDisplay (getOverflow registers) ]
                    , td [] [ text <| flagDisplay (getNegative registers) ]
                    ]
                ]
            , table [ id CyclesTable ]
                [ tr []
                    [ th [] [ text "Cycles" ]
                    ]
                , tr []
                    [ td [] [ text <| toString cycles ]
                    ]
                ]
            ]


type CssIds
    = RegistersElement
    | RegistersTable
    | StatusTable
    | CyclesTable


flagDisplay : Bool -> String
flagDisplay val =
    if val then
        toString 1
    else
        toString 0


styles =
    [ (#) RegistersElement
        [ Css.children
            [ Css.Elements.table
                [ Css.display Css.inlineBlock
                ]
            ]
        ]
    , (#) RegistersTable
        [ Css.descendants
            [ Css.Elements.th
                [ Css.width (Css.ch 6)
                ]
            , Css.Elements.td
                [ Css.textAlign Css.center
                ]
            ]
        ]
    , (#) CyclesTable
        [ Css.textAlign Css.center
        , Css.descendants
            [ Css.Elements.th
                [ Css.width (Css.ch 10)
                ]
            ]
        ]
    ]
