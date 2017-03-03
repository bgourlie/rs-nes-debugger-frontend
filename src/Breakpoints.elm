module Breakpoints
    exposing
        ( icon
        , styles
        , toggleBreakpoint
        , isSet
        , Breakpoints
        )

import Set exposing (Set)
import Html exposing (Html)
import Svg exposing (svg, circle, path)
import Svg.Attributes exposing (width, height, fill, cx, cy, r, d, viewBox)
import Css
import Colors
import Styles


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
