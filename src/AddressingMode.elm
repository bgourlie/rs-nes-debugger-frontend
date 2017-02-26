module AddressingMode exposing (decoder, view, getMemory, AddressingMode)

import Html exposing (span, text, Html)
import Bitwise
import ParseInt exposing (toHex)
import Json.Decode as Json exposing (field, Decoder)
import ParseInt exposing (toHex)
import Byte
import Registers exposing (Registers)
import DebuggerState exposing (Memory, getByte, getWord)


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


getMemory : Memory -> Registers -> AddressingMode -> Maybe ( Int, Int )
getMemory memory registers am =
    case am of
        IndexedIndirect addr ->
            Nothing

        IndirectIndexed addr ->
            let
                -- TODO: for IndirectIndexed, getWord should have zero-page wrapping behavior
                targetAddr =
                    (getWord addr memory) + registers.y

                value =
                    getByte targetAddr memory
            in
                Just ( targetAddr, value )

        ZeroPage addr ->
            Just ( addr, getByte addr memory )

        Absolute addr ->
            Just ( addr, getByte addr memory )

        AbsoluteX addr ->
            Just ( addr + registers.x, getByte (addr + registers.x) memory )

        AbsoluteY addr ->
            Just ( addr + registers.y, getByte (addr + registers.y) memory )

        ZeroPageX addr ->
            Just ( addr + registers.x, getByte (addr + registers.x) memory )

        ZeroPageY addr ->
            Just ( addr + registers.y, getByte (addr + registers.y) memory )

        Indirect addr ->
            let
                targetAddr =
                    getWord addr memory

                value =
                    getWord targetAddr memory
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
                        "-$" ++ toHex (twosCompliment byte)
                    else
                        "$" ++ toHex byte

                Byte.Dec ->
                    toString byte

                Byte.Ascii ->
                    "'" ++ (Byte.asciiValue byte) ++ "'"
    in
        text str


decoder : Decoder AddressingMode
decoder =
    Json.map2 (,)
        (Json.maybe (field "operand" Json.int))
        (field "mode" Json.string)
        |> Json.andThen
            (\( operand, mode ) ->
                case operand of
                    Just op ->
                        case mode of
                            "IndexedIndirect" ->
                                Json.succeed <| IndexedIndirect op

                            "IndirectIndexed" ->
                                Json.succeed <| IndirectIndexed op

                            "ZeroPage" ->
                                Json.succeed <| ZeroPage op

                            "Immediate" ->
                                Json.succeed <| Immediate op

                            "Absolute" ->
                                Json.succeed <| Absolute op

                            "AbsoluteX" ->
                                Json.succeed <| AbsoluteX op

                            "AbsoluteY" ->
                                Json.succeed <| AbsoluteY op

                            "ZeroPageX" ->
                                Json.succeed <| ZeroPageX op

                            "ZeroPageY" ->
                                Json.succeed <| ZeroPageY op

                            "Indirect" ->
                                Json.succeed <| Indirect op

                            "Relative" ->
                                Json.succeed <| Relative op

                            _ ->
                                Json.fail <| "Unexpected addressing mode with operand specified: " ++ mode

                    Nothing ->
                        case mode of
                            "Implied" ->
                                Json.succeed <| Implied

                            "Accumulator" ->
                                Json.succeed <| Accumulator

                            _ ->
                                Json.fail <| "Unexpected mode encountered with no operand specified: " ++ mode
            )


twosCompliment : Int -> Int
twosCompliment val =
    (Bitwise.complement val) + 1
