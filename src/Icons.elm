module Icons exposing (breakpointCircle)

import Html exposing (Html)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (width, height, fill, cx, cy, r)
import Colors


breakpointCircle : Html msg
breakpointCircle =
    svg
        [ width "1.6ch", height "1.6ch" ]
        [ circle
            [ fill Colors.breakpointColor
            , cx "0.8ch"
            , cy "0.8ch"
            , r "0.8ch"
            ]
            []
        ]
