module HexEditor exposing (view, styles)

import Html exposing (table, thead, tbody, td, span, text, tr, th, Html)
import List
import ParseInt exposing (toHex)
import List.Split
import Bitwise
import Css exposing ((#), (.))
import Css.Elements
import CssCommon
import Colors
import Byte
import MemorySnapshot


{ id, class, classList } =
    CssCommon.helpers


type alias Model a =
    { a
        | memory : MemorySnapshot.MemorySnapshot
        , byteFormat : Byte.Format
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
                (th [ class [ OffsetColumn ] ] [ text "Offset" ] :: (List.map (\offset -> th [] [ text <| offsetHeaderDisplay model.byteFormat offset ]) (List.range 0 (bytesPerRow - 1))))
            ]
        , tbody []
            (List.map
                (\( rowOffset, row ) ->
                    tr [ class [ BytesRow ] ]
                        (td [ class [ OffsetColumn, RowOffset ] ] [ text <| offsetDisplay model.byteFormat (startOffset + (rowOffset * bytesPerRow)) ]
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


intoRows : ( Int, List Int ) -> List (List Int)
intoRows ( hash, bytes ) =
    List.Split.chunksOfLeft bytesPerRow (bytes |> List.drop startOffset |> List.take windowSize)


type CssIds
    = HexEditor


type CssClasses
    = RowOffset
    | OffsetColumn
    | BytesRow


offsetHeaderDisplay : Byte.Format -> Int -> String
offsetHeaderDisplay display val =
    case display of
        Byte.Hex ->
            String.padLeft 2 '0' (toHex val)

        Byte.Dec ->
            String.padLeft 2 '0' (toString val)


offsetDisplay : Byte.Format -> Int -> String
offsetDisplay display val =
    case display of
        Byte.Hex ->
            "0x" ++ String.padLeft 4 '0' (toHex val)

        Byte.Dec ->
            String.padLeft 5 '0' (toString val)


styles : List Css.Snippet
styles =
    [ (#) HexEditor
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.position Css.absolute
        , Css.height (Css.pct 100)
        , Css.width (Css.pct 100)
        , Css.backgroundColor Colors.hexEditorBackground
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
    , (.) BytesRow
        [ Css.color Colors.hexEditorByte
        ]
    ]
