module Console exposing (styles, view, addMessage)

import Task exposing (Task)
import Dom
import Dom.Scroll
import Html exposing (Html)
import Css
import CssCommon
import Colors
import Css exposing ((#), (.))


{ id, class, classList } =
    CssCommon.helpers


type alias Model a =
    { a | messages : List ( String, Int ) }



-- TODO: break this up into smaller functions


addMessage : Model a -> (Dom.Error -> msg) -> msg -> String -> ( Model a, Cmd msg )
addMessage model failHandler successHandler message =
    let
        last =
            List.head model.messages

        ( messages, cmd ) =
            let
                newItem =
                    case last of
                        Just msgItem ->
                            let
                                ( msg, repeats ) =
                                    msgItem
                            in
                                if msg == message then
                                    ( msg, repeats + 1 )
                                else
                                    ( message, 0 )

                        Nothing ->
                            ( message, 0 )
            in
                case List.tail model.messages of
                    Just tail ->
                        let
                            ( _, newRepeats ) =
                                newItem
                        in
                            if newRepeats > 0 then
                                ( newItem :: tail, Cmd.none )
                            else
                                let
                                    result =
                                        (\r ->
                                            case r of
                                                Ok _ ->
                                                    successHandler

                                                Err e ->
                                                    failHandler e
                                        )
                                in
                                    -- TODO: "ConsoleContainer" should be a toString of Main.ConsoleContainer
                                    -- refactor so we keep our compile time guarantees
                                    ( newItem :: model.messages, Task.attempt result (Dom.Scroll.toBottom "ConsoleContainer") )

                    Nothing ->
                        ( newItem :: model.messages, Cmd.none )
    in
        ( { model | messages = messages }, cmd )


type CssIds
    = Console


type CssClasses
    = MessageRepeats
    | MessageRepeatsShow


styles : List Css.Snippet
styles =
    [ (#) Console
        [ Css.height (Css.pct 100)
        , Css.padding2 (Css.px 5) (Css.px 10)
        , Css.backgroundColor Colors.consoleBackground
        ]
    , (.) MessageRepeats
        [ Css.display Css.inlineBlock
        , Css.marginLeft (Css.em 0.5)
        , Css.padding2 (Css.em 0.075) (Css.em 0.25)
        , Css.backgroundColor Colors.messageRepeatBackgroundColor
        , Css.borderRadius (Css.px 3)
        , Css.property "visibility" "hidden"
        ]
    , (.) MessageRepeatsShow
        [ Css.property "visibility" "visible"
        ]
    ]


view : Model a -> Html msg
view { messages } =
    Html.ul [ id Console, class [ CssCommon.List ] ]
        (List.map
            (\( msg, repeats ) ->
                Html.li []
                    [ Html.span [] [ Html.text msg ]
                    , Html.span [ messageRepeatsClasses repeats ] [ Html.text <| toString repeats ]
                    ]
            )
            (List.reverse messages)
        )


messageRepeatsClasses repeats =
    if repeats > 0 then
        class [ MessageRepeats, MessageRepeatsShow ]
    else
        class [ MessageRepeats ]
