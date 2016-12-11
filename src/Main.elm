port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button, header, input, fieldset)
import Html.Attributes exposing (disabled, checked, type_, title)
import Html.Events exposing (onClick)
import Dom
import Set exposing (Set)
import Dict exposing (Dict)
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
import ToggleButton
import HexEditor
import MemorySnapshot
import Instruction


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
    , instructionsDisplayed : Int
    , instructionOffsetMap : Instruction.OffsetMap
    , memory : MemorySnapshot.MemorySnapshot
    , registers : Registers.Registers
    , stepState : StepState
    , breakpoints : Breakpoints.Breakpoints
    , byteFormat : Byte.Format
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
            , instructionsDisplayed = 512
            , instructionOffsetMap = Dict.empty
            , memory = ( 0, [] )
            , registers = Registers.new
            , stepState = Off
            , breakpoints = Set.empty
            , byteFormat = Byte.Hex
            }
    in
        ( model, Cmd.none )



-- UPDATE


type AppMessage
    = DebuggerCommandReceiveSuccess DebuggerCommand
    | DebuggerCommandReceiveFail String
    | ToggleBreakpointClick Int
    | ToggleBreakpointRequestSuccess ToggleBreakpoint.Message
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
    | UpdateByteFormat Byte.Format
    | InstructionRequestSuccess (List Instruction.Instruction)
    | InstructionRequestFail Http.Error
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        ToggleAutoStepClicked ->
            handleStepInput AutoStepToggle ( model, Cmd.none )

        DebuggerCommandReceiveSuccess debuggerCommand ->
            handleDebuggerCommand debuggerCommand ( model, Cmd.none )

        DebuggerCommandReceiveFail msg ->
            consoleMessage ("Unable to receive debugger command: " ++ msg) ( model, Cmd.none )

        ToggleBreakpointClick address ->
            ( model, ToggleBreakpoint.request address ToggleBreakpointRequestFail ToggleBreakpointRequestSuccess )

        ToggleBreakpointRequestSuccess resp ->
            let
                breakpointMessage =
                    "Breakpoint "
                        ++ if resp.isSet then
                            "set"
                           else
                            "unset" ++ " @ 0x" ++ toHex resp.offset

                newModel =
                    { model | breakpoints = (Breakpoints.toggle model resp.isSet resp.offset) }
            in
                consoleMessage breakpointMessage ( newModel, Cmd.none )

        ToggleBreakpointRequestFail err ->
            consoleMessage ("Set breakpoint fail: " ++ toString err) ( model, Cmd.none )

        StepClick ->
            handleStepInput SingleStep ( model, Cmd.none )

        StepRequestSuccess resp ->
            handleStepInput StepRequestSucceeded ( model, Cmd.none )

        StepRequestFail err ->
            handleStepInput StepRequestFailed ( model, Cmd.none )

        ContinueClick ->
            ( model, Cmd.none )
                |> handleStepInput AutoStepOff
                |> consoleMessage ("Execution Continued...")
                |> sendContinueRequest

        ContinueRequestSuccess resp ->
            ( model, Cmd.none )

        ContinueRequestFail err ->
            consoleMessage ("Continue request fail: " ++ toString err) ( model, Cmd.none )

        ScrollInstructionIntoView ->
            ( model, scrollElementIntoView <| toString Instruction.CurrentInstruction )

        ScrollConsoleSucceed ->
            ( model, Cmd.none )

        ScrollConsoleFail _ ->
            ( model, Cmd.none )

        UpdateByteFormat byteFormat ->
            ( { model | byteFormat = byteFormat }, Cmd.none )

        InstructionRequestSuccess instructions ->
            ( model, Cmd.none )
                |> consoleMessage ("Received " ++ (toString <| List.length instructions) ++ " instructions")
                |> handleInstructionsResponse instructions

        InstructionRequestFail err ->
            consoleMessage ("Continue request fail: " ++ toString err) ( model, Cmd.none )

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


handleInstructionsResponse : List Instruction.Instruction -> ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
handleInstructionsResponse instructions appInput =
    let
        ( inputModel, inputCmd ) =
            appInput

        instrMap =
            instructions
                |> List.indexedMap (\index instr -> ( instr.offset, index ))
                |> Dict.fromList
    in
        ( { inputModel | instructions = instructions, instructionOffsetMap = instrMap }, inputCmd )


handleStepInput : StepInput -> ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
handleStepInput smInput appInput =
    let
        ( inputModel, inputCmd ) =
            appInput
    in
        case stepStateTransition inputModel.stepState smInput of
            Off ->
                ( { inputModel | stepState = Off }, inputCmd )

            SingleStepping ->
                ( { inputModel | stepState = SingleStepping }, Cmd.batch [ inputCmd, (Step.request StepRequestFail StepRequestSuccess) ] )

            AutoStepping ->
                ( { inputModel | stepState = AutoStepping }, Cmd.batch [ inputCmd, (Step.request StepRequestFail StepRequestSuccess) ] )


onBreakpoint : Model -> Bool
onBreakpoint model =
    Set.member model.registers.pc model.breakpoints


consoleMessage : String -> ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
consoleMessage message appInput =
    Console.addMessage ScrollConsoleFail ScrollConsoleSucceed message appInput


handleDebuggerCommand : DebuggerCommand -> ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
handleDebuggerCommand debuggerCommand appInput =
    case debuggerCommand of
        Break reason snapshot ->
            let
                ( inputModel, inputCmd ) =
                    appInput

                outputModel =
                    { inputModel
                        | registers = snapshot.registers
                        , cycles = snapshot.cycles
                        , memory = snapshot.memory
                    }
            in
                ( outputModel, inputCmd )
                    |> fetchInstructionsIfNeeded
                    |> handleBreakCondition reason snapshot


fetchInstructionsIfNeeded : ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
fetchInstructionsIfNeeded appInput =
    let
        ( inputModel, inputCmd ) =
            appInput
    in
        case inputModel.instructions of
            [] ->
                appInput
                    |> consoleMessage ("Requesting disassembly...")
                    |> sendInstructionRequest

            _ ->
                appInput


sendContinueRequest : ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
sendContinueRequest appInput =
    let
        ( inputModel, inputCmd ) =
            appInput
    in
        ( inputModel, Cmd.batch [ inputCmd, Continue.request ContinueRequestFail ContinueRequestSuccess ] )


sendInstructionRequest : ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
sendInstructionRequest appInput =
    let
        ( inputModel, inputCmd ) =
            appInput
    in
        ( inputModel, Cmd.batch [ inputCmd, Instruction.request InstructionRequestFail InstructionRequestSuccess ] )


handleBreakCondition : DebuggerCommand.BreakReason -> CpuSnapshot.CpuSnapshot -> ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
handleBreakCondition breakReason snapshot appInput =
    case breakReason of
        DebuggerCommand.Breakpoint ->
            breakWithMessage breakReason snapshot ("Hit breakpoint @ 0x" ++ toHex snapshot.registers.pc) appInput

        DebuggerCommand.Trap ->
            breakWithMessage breakReason snapshot ("Trap detected @ 0x" ++ toHex snapshot.registers.pc) appInput

        _ ->
            appInput


breakWithMessage : DebuggerCommand.BreakReason -> CpuSnapshot.CpuSnapshot -> String -> ( Model, Cmd AppMessage ) -> ( Model, Cmd AppMessage )
breakWithMessage breakReason snapshot message appInput =
    -- TODO: This is a poorly named method -- It also turns off auto-step
    appInput
        |> handleStepInput AutoStepOff
        |> consoleMessage message


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ WebSocket.listen wsDebuggerEndpoint <| DebuggerCommand.decode model.memory DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
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
                , ToggleButton.view ToggleAutoStepClicked "autoStepToggle" "Autostep" (autoStepEnabled model)
                ]
            , Byte.toggleDisplayView UpdateByteFormat model
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
