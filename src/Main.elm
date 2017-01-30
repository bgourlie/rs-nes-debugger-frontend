module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button, header, input, fieldset)
import Html.Attributes exposing (disabled, checked, type_, title)
import Html.Events exposing (onClick)
import Dom
import Set exposing (Set)
import Dict exposing (Dict)
import Http
import WebSocket
import Css
import Css.Elements
import ParseInt exposing (toHex)
import DebuggerCommand exposing (BreakReason, DebuggerCommand(Break))
import Instruction
import CpuSnapshot exposing (CpuSnapshot)
import Ports
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
import Styles


{ id, class, classList } =
    Styles.helpers


wsDebuggerEndpoint : String
wsDebuggerEndpoint =
    "ws://localhost:9976"


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { messages : List ( String, Int )
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


init : ( Model, Cmd Msg )
init =
    let
        model =
            { messages = [ ( "Welcome to the rs-nes debugger!", 0 ) ]
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


type Msg
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
    | ScrollInstructionIntoViewClick
    | ScrollConsoleFail Dom.Error
    | ScrollConsoleSucceed
    | UpdateByteFormat Byte.Format
    | InstructionRequestSuccess (List Instruction.Instruction)
    | InstructionRequestFail Http.Error
    | ScrollEventReceived Ports.ScrollEvent
    | ScrollEventDecodeError String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAutoStepClicked ->
            ( model, Cmd.none )
                |> transitionStepState AutoStepToggle

        DebuggerCommandReceiveSuccess debuggerCommand ->
            ( model, Cmd.none )
                |> handleDebuggerCommand debuggerCommand

        DebuggerCommandReceiveFail msg ->
            ( model, Cmd.none )
                |> consoleMessage ("Unable to receive debugger command: " ++ msg)

        ToggleBreakpointClick address ->
            ( model, ToggleBreakpoint.request address ToggleBreakpointRequestFail ToggleBreakpointRequestSuccess )

        ToggleBreakpointRequestSuccess resp ->
            ( { model | breakpoints = (Breakpoints.toggle model resp.isSet resp.offset) }, Cmd.none )

        ToggleBreakpointRequestFail err ->
            consoleMessage ("Set breakpoint fail: " ++ toString err) ( model, Cmd.none )

        StepClick ->
            ( model, Cmd.none )
                |> transitionStepState SingleStep

        StepRequestSuccess resp ->
            ( model, Cmd.none )
                |> transitionStepState StepRequestSucceeded

        StepRequestFail err ->
            ( model, Cmd.none )
                |> consoleMessage "Step request failed"
                |> transitionStepState StepRequestFailed

        ContinueClick ->
            ( model, Cmd.none )
                |> consoleMessage "Execution Continued..."
                |> transitionStepState AutoStepOff
                |> sendContinueRequest

        ContinueRequestSuccess resp ->
            ( model, Cmd.none )

        ContinueRequestFail err ->
            ( model, Cmd.none )
                |> consoleMessage ("Continue request fail: " ++ toString err)

        ScrollInstructionIntoViewClick ->
            ( model, Cmd.none )
                |> scrollElementIntoView (toString Styles.CurrentInstruction)

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
            ( model, Cmd.none )
                |> consoleMessage ("Continue request fail: " ++ toString err)

        ScrollEventReceived e ->
            ( model, Cmd.none )

        ScrollEventDecodeError err ->
            ( model, Cmd.none )
                |> consoleMessage ("ScrollDecodeError: " ++ err)

        NoOp ->
            ( model, Cmd.none )


scrollElementIntoView : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
scrollElementIntoView class appInput =
    appInput
        |> \( inputMessage, inputCmd ) ->
            ( inputMessage, Cmd.batch [ inputCmd, Ports.scrollElementIntoViewCommand class ] )


getNewStepState : StepState -> StepInput -> StepState
getNewStepState currentState input =
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


handleInstructionsResponse : List Instruction.Instruction -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
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


transitionStepState : StepInput -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
transitionStepState smInput appInput =
    let
        ( inputModel, inputCmd ) =
            appInput

        oldStepState =
            inputModel.stepState

        newStepState =
            getNewStepState inputModel.stepState smInput

        stepRequest =
            Step.request StepRequestFail StepRequestSuccess
    in
        appInput
            |> case newStepState of
                Off ->
                    \( outputModel, outputCmd ) ->
                        ( { outputModel | stepState = Off }, outputCmd )

                SingleStepping ->
                    \( outputModel, outputCmd ) ->
                        ( { outputModel | stepState = SingleStepping }, Cmd.batch [ outputCmd, stepRequest ] )

                AutoStepping ->
                    \output ->
                        output
                            |> \( outputModel, outputCmd ) ->
                                ( { outputModel | stepState = AutoStepping }, Cmd.batch [ outputCmd, stepRequest ] )


onBreakpoint : Model -> Bool
onBreakpoint model =
    Set.member model.registers.pc model.breakpoints


consoleMessage : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
consoleMessage message appInput =
    Console.addMessage ScrollConsoleFail ScrollConsoleSucceed message appInput


handleDebuggerCommand : DebuggerCommand -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleDebuggerCommand debuggerCommand appInput =
    case debuggerCommand of
        Break reason snapshot ->
            appInput
                |> fetchInstructionsIfNeeded
                |> handleBreakCondition reason snapshot
                |> \( outputModel, outputCmd ) ->
                    ( { outputModel
                        | registers = snapshot.registers
                        , memory = snapshot.memory
                      }
                    , outputCmd
                    )


fetchInstructionsIfNeeded : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
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


sendContinueRequest : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendContinueRequest appInput =
    let
        ( inputModel, inputCmd ) =
            appInput
    in
        ( inputModel, Cmd.batch [ inputCmd, Continue.request ContinueRequestFail ContinueRequestSuccess ] )


sendInstructionRequest : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendInstructionRequest appInput =
    let
        ( inputModel, inputCmd ) =
            appInput
    in
        ( inputModel, Cmd.batch [ inputCmd, Instruction.request InstructionRequestFail InstructionRequestSuccess ] )


handleBreakCondition : BreakReason -> CpuSnapshot -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleBreakCondition breakReason snapshot appInput =
    case breakReason of
        DebuggerCommand.Breakpoint ->
            appInput
                |> consoleMessage ("Hit breakpoint @ 0x" ++ toHex snapshot.registers.pc)
                |> transitionStepState AutoStepOff

        DebuggerCommand.Trap ->
            appInput
                |> consoleMessage ("Trap detected @ 0x" ++ toHex snapshot.registers.pc)
                |> transitionStepState AutoStepOff

        _ ->
            appInput


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen wsDebuggerEndpoint <|
            DebuggerCommand.decode model.memory DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
        , Ports.scrollEvent <| Ports.mapScrollEvent ScrollEventDecodeError ScrollEventReceived
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ id Styles.Container ]
        [ header []
            [ Registers.view model
            , div [ id Styles.DebuggerButtons ]
                [ button [ class [ Styles.Button ], onClick ContinueClick, title "Continue" ] [ Icons.continue ]
                , button [ class [ Styles.Button ], onClick StepClick, disabled <| autoStepEnabled model, title "Step" ] [ Icons.step ]
                , button [ class [ Styles.Button ], onClick ScrollInstructionIntoViewClick, title "Find Current Instruction" ] [ Icons.magnifyingGlass ]
                , ToggleButton.view ToggleAutoStepClicked "autoStepToggle" "Autostep" (autoStepEnabled model)
                ]
            , Byte.toggleDisplayView UpdateByteFormat model
            ]
        , div [ id Styles.TwoColumn ]
            [ div [ id Styles.LeftColumn ]
                [ div [ id Styles.InstructionsContainer ]
                    [ Instruction.view (\address -> ToggleBreakpointClick address) model
                    ]
                ]
            , div [ id Styles.RightColumn ]
                [ div [ id Styles.ConsoleContainer ]
                    [ Console.view model
                    ]
                , div [ id Styles.HexEditorContainer ]
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


styles : List Css.Snippet
styles =
    [ Styles.id Styles.Container
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
    , Styles.id Styles.TwoColumn
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.property "flex" "1 1 auto"
        , Css.minHeight (Css.px 0)
        , Css.minWidth (Css.px 0)
        ]
    , Styles.id Styles.LeftColumn
        [ Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
        , Css.overflowY Css.auto
        ]
    , Styles.id Styles.RightColumn
        [ Css.displayFlex
        , Css.flex3 (Css.num 2) (Css.num 0) (Css.num 0)
        , Css.flexDirection Css.column
        , Css.overflow Css.auto
        ]
    , Styles.id Styles.ConsoleContainer
        [ Css.overflowY Css.auto
        , Css.backgroundColor Colors.consoleBackground
        , Css.displayFlex
        , Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
        ]
    , Styles.id Styles.HexEditorContainer
        [ Css.displayFlex
        , Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
        , Css.overflowY Css.auto
        , Css.position Css.relative
        ]
    , Styles.id Styles.DebuggerButtons
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
