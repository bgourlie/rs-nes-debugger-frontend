module HexEditor exposing (view, styles, unpackAll)

import Html exposing (table, thead, tbody, td, span, text, tr, th, Html)
import List
import ParseInt exposing (toHex)
import List.Split
import Bitwise
import Css exposing ((#), (.))
import Css.Elements
import CssCommon
import Colors


{ id, class, classList } =
    CssCommon.helpers


type alias Model a =
    { a
        | memory : List Int
    }


bytesPerRow =
    32


windowSize =
    2048


startOffset =
    0x00


view : Model a -> Html msg
view model =
    table [ id HexEditor ]
        [ thead []
            [ tr []
                (th [ class [ OffsetColumn ] ] [ text "Offset" ] :: (List.map (\offset -> th [] [ text <| String.padLeft 2 '0' (toHex offset) ]) (List.range 0 (bytesPerRow - 1))))
            ]
        , tbody []
            (List.map
                (\( rowOffset, row ) ->
                    tr []
                        (td [ class [ OffsetColumn, RowOffset ] ] [ text <| String.padLeft 4 '0' (toHex <| startOffset + (rowOffset * bytesPerRow)) ]
                            :: (List.map
                                    (\byte ->
                                        td [] [ text <| String.padLeft 2 '0' (toHex byte) ]
                                    )
                                    row
                               )
                        )
                )
                (List.map2 (,) (List.range 0 (floor (windowSize / bytesPerRow))) (intoRows model.memory))
            )
        ]


intoRows : List Int -> List (List Int)
intoRows bytes =
    List.Split.chunksOfLeft bytesPerRow (bytes |> List.drop startOffset |> List.take windowSize)


unpackAll : List Int -> List Int
unpackAll packedBytes =
    List.concatMap
        (\packedByte ->
            let
                ( byte1, byte2, byte3, byte4 ) =
                    unpack32 packedByte
            in
                [ byte1, byte2, byte3, byte4 ]
        )
        packedBytes


unpack32 : Int -> ( Int, Int, Int, Int )
unpack32 val =
    let
        mask =
            255
    in
        ( Bitwise.and val mask
        , Bitwise.and (Bitwise.shiftRightBy 8 val) mask
        , Bitwise.and (Bitwise.shiftRightBy 16 val) mask
        , Bitwise.and (Bitwise.shiftRightBy 24 val) mask
        )


type CssIds
    = HexEditor


type CssClasses
    = RowOffset
    | OffsetColumn


styles : List Css.Snippet
styles =
    [ (#) HexEditor
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.position Css.absolute
        , Css.height (Css.pct 100)
        , Css.children
            [ Css.Elements.thead
                [ Css.display Css.block
                , Css.property "flex" "0 1 auto"
                , Css.color Colors.hexEditorOffsetColor
                ]
            , Css.Elements.tbody
                [ Css.display Css.block
                , Css.property "flex" "0 1 auto"
                , Css.overflow Css.auto
                , Css.height (Css.pct 100)
                ]
            ]
        ]
    , (.) RowOffset
        [ Css.fontWeight Css.bold
        , Css.color Colors.hexEditorOffsetColor
        , Css.property "user-select" "none"
        ]
    , (.) OffsetColumn
        [ Css.width (Css.ch 8)
        , Css.textAlign Css.left
        ]
    ]
