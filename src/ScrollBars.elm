module ScrollBars exposing (init, subscriptions, styles, update, Model, Msg)

import Ports
import Css
import Dict exposing (Dict)
import Styles


type alias Model =
    Dict String ScrollProperties


type Msg
    = ScrollEventReceived Ports.ScrollEvent
    | ScrollEventDecodeFail String


type alias ScrollProperties =
    { positionY : Float
    , isDragging : Bool
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    if Dict.isEmpty model then
        Sub.none
    else
        Ports.scrollEvent <| Ports.mapScrollEvent ScrollEventDecodeFail ScrollEventReceived


init : List Styles.Id -> ( Model, Cmd a )
init scrollElementIds =
    let
        model =
            scrollElementIds
                |> List.map (\id -> ( toString id, { positionY = 0, isDragging = False } ))
                |> Dict.fromList

        cmd =
            scrollElementIds
                |> List.map (\id -> (Ports.receiveScrollEventsForCommand <| toString id))
                |> Cmd.batch
    in
        ( model, cmd )


{-| Update the component model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollEventReceived e ->
            model
                |> Dict.update e.elementId (Maybe.andThen (\oldProps -> Just { oldProps | positionY = e.positionY }))
                |> \newModel -> ( newModel, Cmd.none )

        ScrollEventDecodeFail err ->
            ( model, Cmd.none )


styles =
    [ Styles.class Styles.ScrollContainer
        [ Css.position Css.relative
        , Css.overflow Css.hidden
        , Css.height (Css.pct 100)
        , Css.children
            [ Styles.class Styles.ScrollBar
                [ Css.position Css.absolute
                , Css.height (Css.pct 100)
                , Css.width (Css.px 6)
                , Css.right (Css.px 3)
                , Css.padding2 (Css.px 6) (Css.px 0)
                , Css.backgroundColor (Css.hex "#00ff00")
                , Css.children
                    [ Styles.class Styles.ScrollBarHandle
                        [ Css.position Css.absolute
                        , Css.width (Css.pct 100)
                        , Css.top (Css.px 0)
                        , Css.height (Css.px 63)
                        , Css.backgroundColor (Css.hex "#FFFF00")
                        ]
                    , Styles.class Styles.ScrollInnerContainer
                        [ Css.height (Css.pct 100)
                        , Css.overflowX Css.hidden
                        , Css.overflowY Css.scroll
                        , Css.children
                            [ Styles.class Styles.ScrollContentWrapper
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
