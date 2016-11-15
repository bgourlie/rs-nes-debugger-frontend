port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button, header, input)
import Html.Attributes exposing (disabled, checked, type')
import Html.Events exposing (onClick)
import Html.App as App
import Http
import Time exposing (Time, every)
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
import Colors


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
    , autoStepEnabled : Bool
    , autoStepInterval : Float
    }


init : ( Model, Cmd AppMessage )
init =
    ( { messages = [ "Welcome to the rs-nes debugger!" ], instructions = [], registers = Registers.new, autoStepEnabled = False, autoStepInterval = Time.inMilliseconds 20 }, Cmd.none )



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
    | ToggleAutoStep
    | AutoStepTick Time
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        ToggleAutoStep ->
            ( { model | autoStepEnabled = not model.autoStepEnabled }, Cmd.none )

        AutoStepTick time ->
            ( model, Step.request StepRequestFail StepRequestSuccess )

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
            ( model, Cmd.none )

        StepRequestFail err ->
            ( { model | messages = (("Step request fail: " ++ toString err) :: model.messages) }, Cmd.none )

        ContinueClick ->
            ( { model | autoStepEnabled = False }, Continue.request ContinueRequestFail ContinueRequestSuccess )

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
                ( { model | messages = (("Breaking @ 0x" ++ toHex pc) :: model.messages), instructions = snapshot.instructions, registers = snapshot.registers }, Cmd.none )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    let
        autoStepSubscription =
            if model.autoStepEnabled then
                Time.every model.autoStepInterval AutoStepTick
            else
                Sub.none
    in
        Sub.batch
            [ WebSocket.listen wsDebuggerEndpoint <| DebuggerCommand.decode DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
            , autoStepSubscription
            ]



-- VIEW


view : Model -> Html AppMessage
view model =
    div [ id Container ]
        [ header []
            [ Registers.view model.registers
            , div [ id DebuggerButtons ]
                [ button [ onClick StepClick, disabled model.autoStepEnabled ] [ text "Step" ]
                , input [ type' "checkbox", checked model.autoStepEnabled, onClick ToggleAutoStep ] []
                , button [ onClick ContinueClick ] [ text "Continue" ]
                ]
            ]
        , div [ id TwoColumn ]
            [ div [ id InstructionsViewContainer ]
                [ Instruction.view model.registers.pc model.instructions
                ]
            , div [ id ConsoleContainer ]
                [ ul [ id Console, class [ CssCommon.List ] ] (List.map (\msg -> li [] [ text msg ]) (List.reverse model.messages))
                ]
            ]
        ]


type CssIds
    = Container
    | TwoColumn
    | Console
    | DebuggerButtons
    | InstructionsViewContainer
    | ConsoleContainer


styles : List Css.Snippet
styles =
    [ (#) Container
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.height (Css.vh 100)
        , Css.children
            [ Css.Elements.header
                [ Css.property "flex" "0 1 auto"
                , Css.width (Css.pct 100)
                , Css.backgroundColor Colors.headerColor
                , Css.padding (Css.px 5)
                , Css.borderBottom3 (Css.px 1) (Css.solid) Colors.headerBorder
                ]
            ]
        ]
    , (#) TwoColumn
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.property "flex" "1 1 auto"
        ]
    , (#) Console
        [ Css.fontFamily Css.monospace
        , Css.height (Css.pct 100)
        , Css.padding2 (Css.px 5) (Css.px 10)
        , Css.backgroundColor Colors.consoleBackground
        ]
    , (#) InstructionsViewContainer
        [ Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
        , Css.overflowY Css.auto
        ]
    , (#) ConsoleContainer
        [ Css.flex3 (Css.num 2) (Css.num 0) (Css.num 0)
        , Css.overflowY Css.auto
        , Css.backgroundColor Colors.consoleBackground
        ]
    ]
