port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button)
import Html.Events exposing (onClick)
import Html.App as App
import Http
import WebSocket
import Css exposing ((#))
import Css.Elements
import ParseInt exposing (toHex)
import CssCommon
import DebuggerCommand exposing (DebuggerCommand(Break))
import CpuSnapshot
import Instruction
import ToggleBreakpoint
import Continue
import Registers
import Step


{ id, class, classList } =
    CssCommon.helpers



--


wsDebuggerEndpoint : String
wsDebuggerEndpoint =
    "ws://localhost:9976"


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { messages : List String
    , instructions : List Instruction.Model
    , registers : Registers.Model
    }


init : ( Model, Cmd AppMessage )
init =
    ( { messages = [ "Welcome to the rs-nes debugger!" ], instructions = [], registers = Registers.new }, Cmd.none )



-- UPDATE


type AppMessage
    = DebuggerCommandReceiveSuccess DebuggerCommand
    | DebuggerCommandReceiveFail String
    | SetBreakpointClick Int
    | SetBreakpointRequestSuccess ToggleBreakpoint.Model
    | SetBreakpointRequestFail Http.Error
    | StepClick
    | StepRequestSuccess Step.Model
    | StepRequestFail Http.Error
    | ContinueClick
    | ContinueRequestSuccess Continue.Model
    | ContinueRequestFail Http.Error
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        DebuggerCommandReceiveSuccess cmd ->
            handleDebuggerCommand model cmd

        DebuggerCommandReceiveFail msg ->
            ( { model | messages = (("Unable to receive debugger command: " ++ msg) :: model.messages) }, Cmd.none )

        SetBreakpointClick address ->
            ( model, ToggleBreakpoint.request address SetBreakpointRequestFail SetBreakpointRequestSuccess )

        SetBreakpointRequestSuccess resp ->
            ( { model | messages = (("Breakpoint set at " ++ toString resp.address) :: model.messages) }, Cmd.none )

        SetBreakpointRequestFail err ->
            ( { model | messages = (("Set breakpoint fail: " ++ toString err) :: model.messages) }, Cmd.none )

        StepClick ->
            ( model, Step.request StepRequestFail StepRequestSuccess )

        StepRequestSuccess resp ->
            ( { model | messages = ("Stepped!" :: model.messages) }, Cmd.none )

        StepRequestFail err ->
            ( { model | messages = (("Step request fail: " ++ toString err) :: model.messages) }, Cmd.none )

        ContinueClick ->
            ( model, Continue.request ContinueRequestFail ContinueRequestSuccess )

        ContinueRequestSuccess resp ->
            ( { model | messages = ("Continued!" :: model.messages) }, Cmd.none )

        ContinueRequestFail err ->
            ( { model | messages = (("Continue request fail: " ++ toString err) :: model.messages) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleDebuggerCommand : Model -> DebuggerCommand -> ( Model, Cmd AppMessage )
handleDebuggerCommand model cmd =
    case cmd of
        Break snapshot ->
            let
                pc =
                    snapshot.registers.pc
            in
                ( { model | messages = (("Breaking @ " ++ toHex pc) :: model.messages), instructions = snapshot.instructions, registers = snapshot.registers }, Cmd.none )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ WebSocket.listen wsDebuggerEndpoint <| DebuggerCommand.decode DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
        ]



-- VIEW


view : Model -> Html AppMessage
view model =
    div []
        [ button [ onClick StepClick ] [ text "Step" ]
        , button [ onClick ContinueClick ] [ text "Continue" ]
        , div [ id TwoColumn ]
            [ div [ id LeftColumn ]
                [ div []
                    [ div [] [ Registers.view model.registers ]
                    , Instruction.view model.registers.pc model.instructions
                    ]
                ]
            , ul [ id Messages, class [ CssCommon.List ] ] (List.map (\msg -> li [] [ text msg ]) (List.reverse model.messages))
            ]
        ]


type CssIds
    = TwoColumn
    | LeftColumn
    | Messages


styles : List Css.Snippet
styles =
    [ (#) TwoColumn
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.children
            [ Css.Elements.div
                [ Css.padding (Css.px 5)
                ]
            ]
        ]
    , (#) LeftColumn
        [ Css.borderRight3 (Css.px 1) Css.solid (Css.hex "#CCCCCC")
        ]
    , (#) Messages
        [ Css.fontFamily Css.monospace
        ]
    ]
