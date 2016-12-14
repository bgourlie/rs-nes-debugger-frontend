module HexEditor exposing (view, styles)

import Html exposing (table, thead, tbody, td, span, text, tr, th, Html)
import List
import ParseInt exposing (toHex)
import List.Split
import Bitwise
import Css
import Css.Elements
import Styles
import Colors
import Byte
import MemorySnapshot


{ id, class, classList } =
    Styles.helpers


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
    let
        offsetHeaderCells =
            (th [ class [ Styles.OffsetColumn ] ] [ text "Offset" ]
                :: ((bytesPerRow - 1)
                        |> List.range 0
                        |> List.map (\offset -> th [] [ text <| offsetHeaderDisplay model.byteFormat offset ])
                   )
            )
    in
        table [ id Styles.HexEditor ]
            [ thead [] [ tr [] offsetHeaderCells ]
            , tbody [ id Styles.HexEditorBody ] (intoRows model)
            ]


intoRows : Model a -> List (Html msg)
intoRows model =
    let
        ( _, bytes ) =
            model.memory
    in
        bytes
            |> List.drop startOffset
            |> List.take windowSize
            |> List.Split.chunksOfLeft bytesPerRow
            |> List.map2 (,) (List.range 0 (floor (windowSize / bytesPerRow)))
            |> List.map
                (\( rowOffset, row ) ->
                    tr [ class [ Styles.BytesRow ] ]
                        (td [ class [ Styles.OffsetColumn, Styles.RowOffset ] ] [ Byte.view16 model.byteFormat (startOffset + (rowOffset * bytesPerRow)) ]
                            :: (List.map
                                    (\byte ->
                                        td [] [ text <| String.padLeft 2 '0' (toHex byte) ]
                                    )
                                    row
                               )
                        )
                )


offsetHeaderDisplay : Byte.Format -> Int -> String
offsetHeaderDisplay display val =
    case display of
        Byte.Hex ->
            String.padLeft 2 '0' (toHex val)

        Byte.Dec ->
            String.padLeft 2 '0' (toString val)


styles : List Css.Snippet
styles =
    [ Styles.id Styles.HexEditor
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
    , Styles.class Styles.RowOffset
        [ Css.fontWeight Css.bold
        , Css.color Colors.hexEditorOffsetColor
        , Css.property "user-select" "none"
        ]
    , Styles.class Styles.OffsetColumn
        [ Css.width (Css.ch 8)
        , Css.textAlign Css.left
        ]
    , Styles.class Styles.BytesRow
        [ Css.color Colors.hexEditorByte
        ]
    ]
