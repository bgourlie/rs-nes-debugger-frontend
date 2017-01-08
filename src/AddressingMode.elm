module AddressingMode exposing (decoder, view, getMemory, AddressingMode)

import Html exposing (span, text, Html)
import Bitwise
import Json.Decode as Json exposing (field, Decoder)
import ParseInt exposing (toHex)
import Byte
import Registers exposing (Registers)
import MemorySnapshot exposing (getByte, MemorySnapshot)


type AddressingMode
    = IndexedIndirect Int
    | IndirectIndexed Int
    | ZeroPage Int
    | Immediate Int
    | Absolute Int
    | AbsoluteX Int
    | AbsoluteY Int
    | ZeroPageX Int
    | Relative Int
    | Implied
    | Accumulator


getMemory : MemorySnapshot -> Registers -> AddressingMode -> Maybe ( Int, Int )
getMemory memory registers am =
    case am of
        IndexedIndirect baseAddr ->
            getByte (Bitwise.and (baseAddr + registers.x) 0xFF) memory
                |> Maybe.andThen (\addr -> getByte addr memory)
                |> Maybe.map (\targetAddr -> ( targetAddr, Maybe.withDefault 0 (getByte targetAddr memory) ))

        IndirectIndexed baseAddr ->
            getByte baseAddr memory
                |> Maybe.andThen (\addr -> getByte (addr + registers.y) memory)
                |> Maybe.map (\targetAddr -> ( targetAddr, Maybe.withDefault 0 (getByte targetAddr memory) ))

        ZeroPage addr ->
            getByte addr memory
                |> Maybe.map (\byte -> ( addr, byte ))

        Absolute addr ->
            getByte addr memory
                |> Maybe.map (\byte -> ( addr, byte ))

        AbsoluteX addr ->
            getByte (addr + registers.x) memory
                |> Maybe.map (\byte -> ( addr + registers.x, byte ))

        AbsoluteY addr ->
            getByte (addr + registers.y) memory
                |> Maybe.map (\byte -> ( addr + registers.y, byte ))

        ZeroPageX addr ->
            getByte (addr + registers.x) memory
                |> Maybe.map (\byte -> ( addr + registers.x, byte ))

        Relative offset ->
            getByte (registers.pc + offset) memory
                |> Maybe.map (\byte -> ( registers.pc + offset, byte ))

        _ ->
            Nothing


view : Byte.Format -> AddressingMode -> Html msg
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

        Relative addr ->
            relativeView display addr

        Implied ->
            span [] []

        Accumulator ->
            span [] [ text "A" ]


indexedIndirectView : Byte.Format -> Int -> Html msg
indexedIndirectView display addr =
    span []
        [ span [] [ text "(" ]
        , asmByteView Byte.Hex addr
        , span [] [ text ",X)" ]
        ]


indirectIndexedView : Byte.Format -> Int -> Html msg
indirectIndexedView display addr =
    span []
        [ span [] [ text "(" ]
        , asmByteView display addr
        , span [] [ text "),Y" ]
        ]


zeroPageView : Byte.Format -> Int -> Html msg
zeroPageView display addr =
    span []
        [ asmByteView display addr
        ]


immediateView : Byte.Format -> Int -> Html msg
immediateView display addr =
    span []
        [ span [] [ text "#" ]
        , asmByteView display addr
        ]


absoluteView : Byte.Format -> Int -> Html msg
absoluteView display addr =
    span []
        [ asmByteView display addr
        ]


zeroPageXView : Byte.Format -> Int -> Html msg
zeroPageXView display addr =
    span []
        [ asmByteView display addr
        , span [] [ text ",X" ]
        ]


absoluteXView : Byte.Format -> Int -> Html msg
absoluteXView display addr =
    span []
        [ asmByteView display addr
        , span [] [ text ",X" ]
        ]


absoluteYView : Byte.Format -> Int -> Html msg
absoluteYView display addr =
    span []
        [ asmByteView display addr
        , span [] [ text ",Y" ]
        ]


relativeView : Byte.Format -> Int -> Html msg
relativeView display addr =
    span []
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
    in
        span [] [ text str ]


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
