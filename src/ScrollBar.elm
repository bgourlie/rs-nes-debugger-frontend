module ScrollBar
    exposing
        ( styles
        , CssClasses(ScrollContainer, ScrollInnerContainer, ScrollContentWrapper, ScrollBar, ScrollBarHandle)
        )

import Css exposing ((.))
import Dict exposing (Dict)


type CssClasses
    = ScrollContainer
    | ScrollInnerContainer
    | ScrollContentWrapper
    | ScrollBar
    | ScrollBarHandle


type alias Model =
    Dict String ScrollBarModel


type alias ScrollBarModel =
    { scrollPosition : Float
    , isDragging : Bool
    }


styles =
    [ (.) ScrollContainer
        [ Css.position Css.relative
        , Css.overflow Css.hidden
        , Css.height (Css.pct 100)
        , Css.children
            [ (.) ScrollBar
                [ Css.position Css.absolute
                , Css.height (Css.pct 100)
                , Css.width (Css.px 6)
                , Css.right (Css.px 3)
                , Css.padding2 (Css.px 6) (Css.px 0)
                , Css.backgroundColor (Css.hex "#00ff00")
                , Css.children
                    [ (.) ScrollBarHandle
                        [ Css.position Css.absolute
                        , Css.width (Css.pct 100)
                        , Css.top (Css.px 0)
                        , Css.height (Css.px 63)
                        , Css.backgroundColor (Css.hex "#FFFF00")
                        ]
                    , (.) ScrollInnerContainer
                        [ Css.height (Css.pct 100)
                        , Css.overflowX Css.hidden
                        , Css.overflowY Css.scroll
                        , Css.children
                            [ (.) ScrollContentWrapper
                                [ Css.height (Css.pct 100)
                                , Css.overflowY Css.visible
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
