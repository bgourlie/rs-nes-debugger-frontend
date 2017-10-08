module AddressingMode exposing (AddressingMode(..), getTargetOffset, view)

import Byte
import ByteArray exposing (ByteArray)
import Html exposing (Html, span, text)
import ParseInt exposing (toHex)
import Registers exposing (Registers)


type AddressingMode
    = IndexedIndirect Int
    | IndirectIndexed Int
    | ZeroPage Int
    | Immediate Int
    | Absolute Int
    | AbsoluteX Int
    | AbsoluteY Int
    | ZeroPageX Int
    | ZeroPageY Int
    | Indirect Int
    | Relative Int
    | Implied
    | Accumulator



-- TODO: This is something that really needs good tests


getTargetOffset : ByteArray -> Registers -> AddressingMode -> Maybe ( Int, Int )
getTargetOffset bytes registers am =
    case am of
        IndexedIndirect addr ->
            Nothing

        IndirectIndexed addr ->
            let
                -- TODO: for IndirectIndexed, getWord should have zero-page wrapping behavior
                targetAddr =
                    ByteArray.getWord addr bytes + registers.y

                value =
                    ByteArray.getByte targetAddr bytes
            in
            Just ( targetAddr, value )

        ZeroPage addr ->
            Just ( addr, ByteArray.getByte addr bytes )

        Absolute addr ->
            Just ( addr, ByteArray.getByte addr bytes )

        AbsoluteX addr ->
            Just ( addr + registers.x, ByteArray.getByte (addr + registers.x) bytes )

        AbsoluteY addr ->
            Just ( addr + registers.y, ByteArray.getByte (addr + registers.y) bytes )

        ZeroPageX addr ->
            Just ( addr + registers.x, ByteArray.getByte (addr + registers.x) bytes )

        ZeroPageY addr ->
            Just ( addr + registers.y, ByteArray.getByte (addr + registers.y) bytes )

        Indirect addr ->
            let
                targetAddr =
                    ByteArray.getWord addr bytes

                value =
                    ByteArray.getWord targetAddr bytes
            in
            Just ( targetAddr, value )

        _ ->
            Nothing


view : Byte.Format -> AddressingMode -> List (Html msg)
view display am =
    case am of
        IndexedIndirect addr ->
            indexedIndirectView display addr

        IndirectIndexed addr ->
            indirectIndexedView display addr

        ZeroPage addr ->
            zeroPageView display addr

        Immediate addr ->
            immediateView display addr

        Absolute addr ->
            absoluteView display addr

        AbsoluteX addr ->
            absoluteXView display addr

        AbsoluteY addr ->
            absoluteYView display addr

        ZeroPageX addr ->
            zeroPageXView display addr

        ZeroPageY addr ->
            zeroPageYView display addr

        Indirect addr ->
            indirectView display addr

        Relative addr ->
            relativeView display addr

        Implied ->
            []

        Accumulator ->
            [ text "A" ]


indexedIndirectView : Byte.Format -> Int -> List (Html msg)
indexedIndirectView display addr =
    [ text "("
    , asmByteView Byte.Hex addr
    , text ",X)"
    ]


indirectIndexedView : Byte.Format -> Int -> List (Html msg)
indirectIndexedView display addr =
    [ text "("
    , asmByteView display addr
    , text "),Y"
    ]


indirectView : Byte.Format -> Int -> List (Html msg)
indirectView display addr =
    [ text "("
    , asmByteView display addr
    , text ")"
    ]


zeroPageView : Byte.Format -> Int -> List (Html msg)
zeroPageView display addr =
    [ asmByteView display addr
    ]


immediateView : Byte.Format -> Int -> List (Html msg)
immediateView display addr =
    [ span [] [ text "#" ]
    , asmByteView display addr
    ]


absoluteView : Byte.Format -> Int -> List (Html msg)
absoluteView display addr =
    [ asmByteView display addr
    ]


zeroPageXView : Byte.Format -> Int -> List (Html msg)
zeroPageXView display addr =
    [ asmByteView display addr
    , text ",X"
    ]


zeroPageYView : Byte.Format -> Int -> List (Html msg)
zeroPageYView display addr =
    [ asmByteView display addr
    , text ",Y"
    ]


absoluteXView : Byte.Format -> Int -> List (Html msg)
absoluteXView display addr =
    [ asmByteView display addr
    , text ",X"
    ]


absoluteYView : Byte.Format -> Int -> List (Html msg)
absoluteYView display addr =
    [ asmByteView display addr
    , text ",Y"
    ]


relativeView : Byte.Format -> Int -> List (Html msg)
relativeView display addr =
    [ asmByteView display addr
    ]



-- We use a special byte for for 6502 assembly, which prefixes hex values with $ instead of 0x


asmByteView : Byte.Format -> Int -> Html msg
asmByteView display byte =
    let
        str =
            case display of
                Byte.Hex ->
                    -- Correctly display negative hex values (accommodates relative addressing)
                    if byte < 0 then
                        "-$" ++ toHex byte
                    else
                        "$" ++ toHex byte

                Byte.Dec ->
                    toString byte

                Byte.Ascii ->
                    "'" ++ Byte.asciiValue byte ++ "'"
    in
    text str
