module AddressingMode exposing (decoder, view, AddressingMode)

import Html exposing (span, text, Html)
import Bitwise
import Json.Decode as Json exposing (field, Decoder)
import ParseInt exposing (toHex)
import Byte


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


view : Byte.Display -> AddressingMode -> Html msg
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


indexedIndirectView : Byte.Display -> Int -> Html msg
indexedIndirectView display addr =
    span []
        [ span [] [ text "(" ]
        , asmByteView Byte.Hex addr
        , span [] [ text ",X)" ]
        ]


indirectIndexedView : Byte.Display -> Int -> Html msg
indirectIndexedView display addr =
    span []
        [ span [] [ text "(" ]
        , asmByteView display addr
        , span [] [ text "),Y" ]
        ]


zeroPageView : Byte.Display -> Int -> Html msg
zeroPageView display addr =
    span []
        [ asmByteView display addr
        ]


immediateView : Byte.Display -> Int -> Html msg
immediateView display addr =
    span []
        [ span [] [ text "#" ]
        , asmByteView display addr
        ]


absoluteView : Byte.Display -> Int -> Html msg
absoluteView display addr =
    span []
        [ asmByteView display addr
        ]


zeroPageXView : Byte.Display -> Int -> Html msg
zeroPageXView display addr =
    span []
        [ asmByteView display addr
        , span [] [ text ",X" ]
        ]


absoluteXView : Byte.Display -> Int -> Html msg
absoluteXView display addr =
    span []
        [ asmByteView display addr
        , span [] [ text ",X" ]
        ]


absoluteYView : Byte.Display -> Int -> Html msg
absoluteYView display addr =
    span []
        [ asmByteView display addr
        , span [] [ text ",Y" ]
        ]


relativeView : Byte.Display -> Int -> Html msg
relativeView display addr =
    span []
        [ asmByteView display addr
        ]



-- We use a special byte for for 6502 assembly, which prefixes hex values with $ instead of 0x


asmByteView : Byte.Display -> Int -> Html msg
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
