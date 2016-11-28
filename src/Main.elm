port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button, header, input, fieldset)
import Html.Attributes exposing (disabled, checked, type_, title)
import Html.Events exposing (onClick)
import Dom
import Set exposing (Set)
import Http
import WebSocket
import Css exposing ((#))
import Css.Elements
import ParseInt exposing (toHex)
import CssCommon
import DebuggerCommand exposing (DebuggerCommand(Break))
import Instruction
import CpuSnapshot
import ToggleBreakpoint
import Continue
import Registers
import Step
import Console
import Colors
import Byte
import Breakpoints
import Icons
import Toggle
import HexEditor


{ id, class, classList } =
    CssCommon.helpers


wsDebuggerEndpoint : String
wsDebuggerEndpoint =
    "ws://localhost:9976"


main : Program Never Model AppMessage
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port scrollElementIntoView : String -> Cmd msg



-- MODEL


type alias Model =
    { messages : List ( String, Int )
    , cycles : Int
    , instructions : List Instruction.Instruction
    , memory : List Int
    , registers : Registers.Registers
    , stepState : StepState
    , breakpoints : Set Int
    , byteDisplay : Byte.Display
    }


type StepState
    = Off
    | SingleStepping
    | AutoStepping


type StepInput
    = SingleStep
    | AutoStepToggle
    | AutoStepOff
    | StepRequestSucceeded
    | StepRequestFailed


init : ( Model, Cmd AppMessage )
init =
    let
        model =
            { messages = [ ( "Welcome to the rs-nes debugger!", 0 ) ]
            , cycles = 0
            , instructions = []
            , memory = []
            , registers = Registers.new
            , stepState = Off
            , breakpoints = Set.empty
            , byteDisplay = Byte.Hex
            }
    in
        ( model, Cmd.none )



-- UPDATE


type AppMessage
    = DebuggerCommandReceiveSuccess DebuggerCommand
    | DebuggerCommandReceiveFail String
    | ToggleBreakpointClick Int
    | ToggleBreakpointRequestSuccess ToggleBreakpoint.Model
    | ToggleBreakpointRequestFail Http.Error
    | StepClick
    | StepRequestSuccess Step.Model
    | StepRequestFail Http.Error
    | ContinueClick
    | ContinueRequestSuccess Continue.Model
    | ContinueRequestFail Http.Error
    | ToggleAutoStepClicked
    | ScrollInstructionIntoView
    | ScrollConsoleFail Dom.Error
    | ScrollConsoleSucceed
    | UpdateByteDisplay Byte.Display
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        ToggleAutoStepClicked ->
            handleStepInput AutoStepToggle model

        DebuggerCommandReceiveSuccess cmd ->
            handleDebuggerCommand model cmd

        DebuggerCommandReceiveFail msg ->
            let
                ( newModel, cmd ) =
                    Console.addMessage model ScrollConsoleFail ScrollConsoleSucceed ("Unable to receive debugger command: " ++ msg)
            in
                ( newModel, cmd )

        ToggleBreakpointClick address ->
            ( model, ToggleBreakpoint.request address ToggleBreakpointRequestFail ToggleBreakpointRequestSuccess )

        ToggleBreakpointRequestSuccess resp ->
            let
                verb =
                    if resp.isSet then
                        "set"
                    else
                        "unset"

                ( newModel, cmd ) =
                    Console.addMessage model ScrollConsoleFail ScrollConsoleSucceed ("Breakpoint " ++ verb ++ " @ 0x" ++ toHex resp.offset)
            in
                ( { newModel | breakpoints = (Breakpoints.toggle model resp.isSet resp.offset) }, cmd )

        ToggleBreakpointRequestFail err ->
            let
                ( newModel, cmd ) =
                    Console.addMessage model ScrollConsoleFail ScrollConsoleSucceed ("Set breakpoint fail: " ++ toString err)
            in
                ( newModel, cmd )

        StepClick ->
            handleStepInput SingleStep model

        StepRequestSuccess resp ->
            handleStepInput StepRequestSucceeded model

        StepRequestFail err ->
            handleStepInput StepRequestFailed model

        ContinueClick ->
            let
                ( handleStepModel, handleStepCmd ) =
                    handleStepInput AutoStepOff model

                ( newModel, addMessageCmd ) =
                    Console.addMessage handleStepModel ScrollConsoleFail ScrollConsoleSucceed ("Execution Continued...")
            in
                ( newModel, Cmd.batch [ handleStepCmd, addMessageCmd, Continue.request ContinueRequestFail ContinueRequestSuccess ] )

        ContinueRequestSuccess resp ->
            ( model, Cmd.none )

        ContinueRequestFail err ->
            let
                ( newModel, cmd ) =
                    Console.addMessage model ScrollConsoleFail ScrollConsoleSucceed ("Continue request fail: " ++ toString err)
            in
                ( newModel, cmd )

        ScrollInstructionIntoView ->
            ( model, scrollElementIntoView <| toString Instruction.CurrentInstruction )

        ScrollConsoleSucceed ->
            ( model, Cmd.none )

        ScrollConsoleFail _ ->
            ( model, Cmd.none )

        UpdateByteDisplay byteDisplay ->
            ( { model | byteDisplay = byteDisplay }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


stepStateTransition : StepState -> StepInput -> StepState
stepStateTransition currentState input =
    case currentState of
        Off ->
            case input of
                SingleStep ->
                    SingleStepping

                AutoStepToggle ->
                    AutoStepping

                _ ->
                    Off

        SingleStepping ->
            case input of
                _ ->
                    Off

        AutoStepping ->
            case input of
                StepRequestSucceeded ->
                    AutoStepping

                SingleStep ->
                    SingleStepping

                _ ->
                    Off


handleStepInput : StepInput -> Model -> ( Model, Cmd AppMessage )
handleStepInput input model =
    case stepStateTransition model.stepState input of
        Off ->
            ( { model | stepState = Off }, Cmd.none )

        SingleStepping ->
            ( { model | stepState = SingleStepping }, Step.request StepRequestFail StepRequestSuccess )

        AutoStepping ->
            ( { model | stepState = AutoStepping }, Step.request StepRequestFail StepRequestSuccess )


onBreakpoint : Model -> Bool
onBreakpoint model =
    Set.member model.registers.pc model.breakpoints


handleDebuggerCommand : Model -> DebuggerCommand -> ( Model, Cmd AppMessage )
handleDebuggerCommand model debuggerCommand =
    case debuggerCommand of
        Break reason snapshot ->
            let
                ( newModel, cmd ) =
                    handleBreakCondition model reason snapshot
            in
                ( { newModel
                    | instructions = snapshot.instructions
                    , registers = snapshot.registers
                    , cycles = snapshot.cycles
                    , memory = HexEditor.unpackAll snapshot.memory
                  }
                , cmd
                )


handleBreakCondition : Model -> DebuggerCommand.BreakReason -> CpuSnapshot.Model -> ( Model, Cmd AppMessage )
handleBreakCondition model breakReason snapshot =
    case breakReason of
        DebuggerCommand.Breakpoint ->
            breakWithMessage model breakReason snapshot ("Hit breakpoint @ 0x" ++ toHex snapshot.registers.pc)

        DebuggerCommand.Trap ->
            breakWithMessage model breakReason snapshot ("Trap detected @ 0x" ++ toHex snapshot.registers.pc)

        _ ->
            ( model, Cmd.none )


breakWithMessage : Model -> DebuggerCommand.BreakReason -> CpuSnapshot.Model -> String -> ( Model, Cmd AppMessage )
breakWithMessage model breakReason snapshot message =
    -- TODO: This is a poorly named method -- It also turns off auto-step
    let
        ( postStepModel, postStepCmd ) =
            handleStepInput AutoStepOff model

        ( postMessageModel, postMessageCmd ) =
            Console.addMessage postStepModel ScrollConsoleFail ScrollConsoleSucceed message
    in
        ( postMessageModel, Cmd.batch [ postStepCmd, postMessageCmd ] )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ WebSocket.listen wsDebuggerEndpoint <| DebuggerCommand.decode DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
        ]



-- VIEW


view : Model -> Html AppMessage
view model =
    div [ id Container ]
        [ header []
            [ Registers.view model
            , div [ id DebuggerButtons ]
                [ button [ class [ CssCommon.Button ], onClick ContinueClick, title "Continue" ] [ Icons.continue ]
                , button [ class [ CssCommon.Button ], onClick StepClick, disabled <| autoStepEnabled model, title "Step" ] [ Icons.step ]
                , button [ class [ CssCommon.Button ], onClick ScrollInstructionIntoView, title "Find Current Instruction" ] [ Icons.magnifyingGlass ]
                , Toggle.view ToggleAutoStepClicked "autoStepToggle" "Autostep" (autoStepEnabled model)
                ]
            , Byte.toggleDisplayView UpdateByteDisplay model
            ]
        , div [ id TwoColumn ]
            [ div [ id InstructionsContainer ]
                [ Instruction.view (\address -> ToggleBreakpointClick address) model
                ]
            , div [ id RightColumn ]
                [ div [ id ConsoleContainer ]
                    [ Console.view model
                    ]
                , div [ id HexEditorContainer ]
                    [ HexEditor.view model
                    ]
                ]
            ]
        ]


autoStepEnabled : Model -> Bool
autoStepEnabled model =
    case model.stepState of
        AutoStepping ->
            True

        _ ->
            False


type CssIds
    = Container
    | TwoColumn
    | DebuggerButtons
    | InstructionsContainer
    | ConsoleContainer
    | HexEditorContainer
    | RightColumn


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
                , Css.borderBottom3 (Css.px 1) (Css.solid) Colors.headerBorder
                , Css.padding (Css.px 5)
                , Css.boxShadow5 (Css.px 0) (Css.px 2) (Css.px 2) (Css.px -2) (Css.rgba 0 0 0 0.4)
                , Css.children
                    [ Css.everything
                        [ Css.display Css.inlineBlock
                        , Css.marginLeft (Css.px 10)
                        , Css.firstChild
                            [ Css.marginLeft (Css.px 0)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , (#) TwoColumn
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.property "flex" "1 1 auto"
        ]
    , (#) InstructionsContainer
        [ Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
        , Css.overflowY Css.auto
        ]
    , (#) RightColumn
        [ Css.displayFlex
        , Css.flex3 (Css.num 2) (Css.num 0) (Css.num 0)
        , Css.flexDirection Css.column
        , Css.overflow Css.auto
        ]
    , (#) ConsoleContainer
        [ Css.overflowY Css.auto
        , Css.backgroundColor Colors.consoleBackground
        , Css.displayFlex
        , Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
        ]
    , (#) HexEditorContainer
        [ Css.displayFlex
        , Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
        , Css.overflowY Css.auto
        , Css.position Css.relative
        ]
    , (#) DebuggerButtons
        [ Css.children
            [ Css.everything
                [ Css.marginLeft (Css.px 5)
                , Css.first
                    [ Css.marginLeft (Css.px 0)
                    ]
                ]
            ]
        ]
    ]
