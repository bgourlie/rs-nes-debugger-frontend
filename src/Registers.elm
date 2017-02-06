module Registers exposing (new, decoder, Registers, view)

import Html exposing (div, ul, li, h4, text, table, tr, td, th, Html)
import Html.Attributes exposing (title, colspan)
import Bitwise exposing (and)
import Json.Decode as Json exposing (Decoder, field)
import Styles
import Byte


{ id, class, classList } =
    Styles.helpers


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


getBreak : Registers -> Bool
getBreak model =
    and model.stat 0x10 > 0


getUnused : Registers -> Bool
getUnused model =
    and model.stat 0x20 > 0


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
        table []
            [ tr []
                [ th [ title "Program Counter" ] [ text "PC" ]
                , th [ title "Stack Pointer" ] [ text "SP" ]
                , th [ title "Accumulator" ] [ text "ACC" ]
                , th [ title "Index (X)" ] [ text "X" ]
                , th [ title "Index (Y)" ] [ text "Y" ]
                , th [ title "Status Flags" ] [ text "NV-BDIZC" ]
                , th [] [ text "Cycles" ]
                ]
            , tr []
                [ td [] [ Byte.view16 display registers.pc ]
                , td [] [ Byte.view8 display registers.sp ]
                , td [] [ Byte.view8 display registers.acc ]
                , td [] [ Byte.view8 display registers.x ]
                , td [] [ Byte.view8 display registers.y ]
                , td []
                    [ text <|
                        flagDisplay (getNegative registers)
                            ++ flagDisplay (getOverflow registers)
                            ++ flagDisplay (getUnused registers)
                            ++ flagDisplay (getBreak registers)
                            ++ flagDisplay (getDecimal registers)
                            ++ flagDisplay (getInterrupt registers)
                            ++ flagDisplay (getZero registers)
                            ++ flagDisplay (getCarry registers)
                    ]
                , td [] [ text <| toString cycles ]
                ]
            ]


flagDisplay : Bool -> String
flagDisplay val =
    if val then
        toString 1
    else
        toString 0
