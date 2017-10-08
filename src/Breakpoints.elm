module Breakpoints
    exposing
        ( Breakpoints
        , icon
        , isSet
        , styles
        , toggleBreakpoint
        )

import Colors
import Css
import Html exposing (Html)
import Set exposing (Set)
import Styles
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, r, viewBox, width)


{ id, class, classList } =
    Styles.helpers


type alias Breakpoints =
    Set Int


type alias Model a =
    { a | breakpoints : Breakpoints }


toggleBreakpoint : Model a -> Bool -> Int -> Breakpoints
toggleBreakpoint model isSet offset =
    if isSet then
        Set.insert offset model.breakpoints
    else
        Set.remove offset model.breakpoints


isSet : Breakpoints -> Int -> Bool
isSet breakpoints offset =
    Set.member offset breakpoints


icon : Html msg
icon =
    svg
        [ Svg.Attributes.class <| toString Styles.BreakpointIcon, viewBox "0 0 20 20" ]
        [ circle
            [ fill Colors.breakpointColor
            , cx "10"
            , cy "10"
            , r "10"
            ]
            []
        ]


styles : List Css.Snippet
styles =
    [ Styles.class Styles.BreakpointIcon
        [ Css.height (Css.ch 1.6)
        , Css.width (Css.ch 1.6)
        ]
    ]
