module Console exposing (styles, view, addMessage)

import Task exposing (Task)
import Css
import Dom
import Dom.Scroll
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Css
import Css.Elements
import Styles
import Colors


{ id, class, classList } =
    Styles.helpers


type alias Model a =
    { a
        | messages : List ( String, Int )
        , consoleInput : String
    }



-- TODO: break this up into smaller functions


addMessage : (Dom.Error -> msg) -> msg -> String -> ( Model a, Cmd msg ) -> ( Model a, Cmd msg )
addMessage failHandler successHandler message appInput =
    let
        ( inputModel, inputCmd ) =
            appInput

        last =
            List.head inputModel.messages

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
                case List.tail inputModel.messages of
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
                                    ( newItem :: inputModel.messages, Task.attempt result (Dom.Scroll.toBottom <| toString Styles.ConsoleLines) )

                    Nothing ->
                        ( newItem :: inputModel.messages, Cmd.none )
    in
        ( { inputModel | messages = messages }, Cmd.batch [ inputCmd, cmd ] )


styles : List Css.Snippet
styles =
    [ Styles.id Styles.Console
        [ Css.displayFlex
        , Css.overflowX Css.hidden
        , Css.overflowY Css.hidden
        , Css.flexDirection Css.column
        , Css.width (Css.pct 100)
        , Css.padding2 (Css.em 0.3) (Css.em 0.5)
        , Css.backgroundColor Colors.consoleBackground
        , Css.descendants
            [ Styles.id Styles.ConsoleLines
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.overflowY Css.scroll
                , Css.marginBottom (Css.em 0.3)
                , Css.children
                    [ Styles.class Styles.ConsoleLine
                        [ Css.paddingBottom (Css.em 0.1)
                        ]
                    ]
                ]
            , Styles.id Styles.ConsoleInput
                [ Css.width (Css.pct 100)
                , Css.marginTop Css.auto
                , Css.outline Css.none
                , Css.border (Css.px 0)
                , Css.fontFamily Css.monospace
                , Css.backgroundColor Colors.consoleInputBackground
                , Css.color Colors.consoleInputText
                , Css.fontSize (Css.em 1)
                , Css.padding2 (Css.em 0.2) (Css.em 0.4)
                ]
            , Styles.class Styles.MessageRepeats
                [ Css.display Css.inlineBlock
                , Css.marginLeft (Css.em 0.5)
                , Css.padding2 (Css.em 0.075) (Css.em 0.25)
                , Css.backgroundColor Colors.messageRepeatBackgroundColor
                , Css.borderRadius (Css.pct 50)
                , Css.fontSize (Css.pct 80)
                , Css.property "visibility" "hidden"
                ]
            , Styles.class Styles.MessageRepeatsShow
                [ Css.property "visibility" "visible"
                ]
            ]
        ]
    ]


view : msg -> (String -> msg) -> msg -> Model a -> Html msg
view nop consoleInputUpdated consoleInputSubmitted { messages, consoleInput } =
    Html.div [ id Styles.Console ]
        [ Html.div [ id Styles.ConsoleLines ]
            (messages
                |> List.map
                    (\( msg, repeats ) ->
                        Html.div [ class [ Styles.ConsoleLine ] ]
                            [ Html.span [] [ Html.text msg ]
                            , Html.span [ messageRepeatsClasses repeats ] [ Html.text <| toString repeats ]
                            ]
                    )
                |> List.reverse
            )
        , Html.input
            [ id Styles.ConsoleInput
            , Html.Attributes.type_ "text"
            , Html.Attributes.placeholder "Enter debugger commands here..."
            , Html.Events.onInput consoleInputUpdated
            , Html.Attributes.value consoleInput
            , onEnter nop consoleInputSubmitted
            ]
            []
        ]


onEnter : msg -> msg -> Html.Attribute msg
onEnter nop consoleInputSubmitted =
    Html.Events.on "keyup"
        (Json.map
            (\keyCode ->
                case keyCode of
                    13 ->
                        consoleInputSubmitted

                    _ ->
                        nop
            )
            Html.Events.keyCode
        )


messageRepeatsClasses : Int -> Html.Attribute msg
messageRepeatsClasses repeats =
    if repeats > 0 then
        class [ Styles.MessageRepeats, Styles.MessageRepeatsShow ]
    else
        class [ Styles.MessageRepeats ]
