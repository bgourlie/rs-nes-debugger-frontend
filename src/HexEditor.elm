module HexEditor exposing (view, styles)

import Html exposing (table, thead, tbody, td, span, text, tr, th, Html)
import List
import ParseInt exposing (toHex)
import List.Split
import Css
import Css.Elements
import Styles
import Colors
import Byte
import ByteArray
import Memory


{ id, class, classList } =
    Styles.helpers


type alias Model a =
    { a
        | memory : Memory.Memory
        , memoryViewOffset : Int
        , offsetByteFormat : Byte.Format
        , memoryByteFormat : Byte.Format
    }


bytesPerRow : Int
bytesPerRow =
    32


windowSize : Int
windowSize =
    2048


view : Model a -> Html msg
view model =
    let
        offsetHeaderCells =
            (th [ class [ Styles.OffsetColumn ] ] [ text "Offset" ]
                :: ((bytesPerRow - 1)
                        |> List.range 0
                        |> List.map (\offset -> th [] [ offsetHeaderDisplay model offset ])
                   )
            )
    in
        table [ id Styles.HexEditor ]
            [ thead [] [ tr [] offsetHeaderCells ]
            , tbody [ id Styles.HexEditorBody ] (intoRows model)
            ]


offsetHeaderDisplay : Model a -> Int -> Html msg
offsetHeaderDisplay model val =
    let
        padding =
            case model.memoryByteFormat of
                Byte.Dec ->
                    3

                _ ->
                    2
    in
        case model.offsetByteFormat of
            Byte.Dec ->
                Html.text (String.padLeft padding '0' (toString val))

            _ ->
                -- default to hex
                Html.text (String.padLeft padding '0' (toHex val))


intoRows : Model a -> List (Html msg)
intoRows model =
    let
        ( _, bytes ) =
            model.memory

        startOffset =
            model.memoryViewOffset
    in
        bytes
            |> ByteArray.slice startOffset (startOffset + windowSize)
            |> ByteArray.toList
            |> List.Split.chunksOfLeft bytesPerRow
            |> List.map2 (,) (List.range 0 (floor (toFloat windowSize / toFloat bytesPerRow)))
            |> List.map
                (\( rowOffset, row ) ->
                    let
                        rowOffset1 =
                            startOffset + (rowOffset * bytesPerRow)
                    in
                        tr [ class [ Styles.BytesRow ] ]
                            (td [ class [ Styles.OffsetColumn, Styles.RowOffset ] ] [ offsetView model.offsetByteFormat rowOffset1 ]
                                :: (List.map
                                        (\byte ->
                                            td [] [ memoryView model byte ]
                                        )
                                        row
                                   )
                            )
                )


offsetView : Byte.Format -> Int -> Html msg
offsetView display byte =
    let
        str =
            case display of
                Byte.Dec ->
                    String.padLeft 5 '0' (toString byte)

                _ ->
                    -- Default to hex display
                    "0x" ++ String.padLeft 4 '0' (toHex byte)
    in
        text str


memoryView : Model a -> Int -> Html msg
memoryView model byte =
    let
        str =
            case model.memoryByteFormat of
                Byte.Dec ->
                    String.padLeft 3 '0' (toString byte)

                Byte.Hex ->
                    -- Default to hex display
                    String.padLeft 2 '0' (toHex byte)

                Byte.Ascii ->
                    "." ++ (Byte.asciiValue byte)
    in
        text str


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
                , Css.overflow Css.hidden
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
        [ Css.width (Css.ch 9)
        , Css.textAlign Css.left
        ]
    , Styles.class Styles.BytesRow
        [ Css.color Colors.hexEditorByte
        ]
    ]
