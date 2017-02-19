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
import DebuggerCommand exposing (BreakReason, CrashReason, DebuggerCommand(..), crashReasonToString)
import Instruction
import CpuSnapshot exposing (CpuSnapshot)
import Ports
import ToggleBreakpoint
import Continue
import Registers
import Step
import DebuggerState
import Console
import Colors
import Byte
import Breakpoints
import Icons
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
    { debuggerState : DebuggerState.DebuggerState
    , messages : List ( String, Int )
    , cycles : Int
    , instructions : List Instruction.Instruction
    , instructionsDisplayed : Int
    , instructionOffsetMap : Instruction.OffsetMap
    , memory : MemorySnapshot.MemorySnapshot
    , registers : Registers.Registers
    , breakpoints : Breakpoints.Breakpoints
    , byteFormat : Byte.Format
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { debuggerState = DebuggerState.NotConnected
            , messages = [ ( "Welcome to the rs-nes debugger!", 0 ) ]
            , cycles = 0
            , instructions = []
            , instructionsDisplayed = 512
            , instructionOffsetMap = Dict.empty
            , memory = ( 0, [] )
            , registers = Registers.new
            , breakpoints = Set.empty
            , byteFormat = Byte.Hex
            }
    in
        ( model, Cmd.none )



-- UPDATE


type Msg
    = DebuggerConnectionOpened String
    | DebuggerConnectionClosed String
    | DebuggerCommandReceiveSuccess DebuggerCommand
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
    | ScrollInstructionIntoViewClick
    | ScrollConsoleFail Dom.Error
    | ScrollConsoleSucceed
    | UpdateByteFormat Byte.Format
    | InstructionRequestSuccess (List Instruction.Instruction)
    | InstructionRequestFail Http.Error
    | UnknownState ( DebuggerState.DebuggerState, DebuggerState.Input )
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebuggerConnectionOpened name ->
            ( model, Cmd.none )
                |> transitionDebuggerState DebuggerState.Connect
                |> clearCpuState
                |> consoleMessage ("Connected to debugger at " ++ name)

        DebuggerConnectionClosed _ ->
            ( model, Cmd.none )
                |> transitionDebuggerState DebuggerState.Disconnect
                |> consoleMessage ("Disconnected from debugger")

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
                |> transitionDebuggerState DebuggerState.Step

        StepRequestSuccess resp ->
            ( model, Cmd.none )
                |> transitionDebuggerState (DebuggerState.StepRequestComplete DebuggerState.Success)

        StepRequestFail err ->
            ( model, Cmd.none )
                |> transitionDebuggerState (DebuggerState.StepRequestComplete DebuggerState.Fail)
                |> consoleMessage "Step request failed"

        ContinueClick ->
            ( model, Cmd.none )
                |> transitionDebuggerState DebuggerState.Continue
                |> consoleMessage "Continuing execution..."

        ContinueRequestSuccess resp ->
            ( model, Cmd.none )
                |> transitionDebuggerState (DebuggerState.ContinueRequestComplete DebuggerState.Success)

        ContinueRequestFail err ->
            ( model, Cmd.none )
                |> transitionDebuggerState (DebuggerState.ContinueRequestComplete DebuggerState.Fail)
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

        UnknownState ( state, input ) ->
            ( model, Cmd.none )
                |> consoleMessage ("Uh oh, the following state transition wasn't handled: " ++ (toString input) ++ " -> " ++ (toString state))

        NoOp ->
            ( model, Cmd.none )


transitionDebuggerState : DebuggerState.Input -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
transitionDebuggerState smInput appInput =
    DebuggerState.transition StepRequestFail StepRequestSuccess ContinueRequestFail ContinueRequestSuccess UnknownState smInput appInput


clearCpuState : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
clearCpuState appInput =
    let
        ( model, cmd ) =
            appInput

        newModel =
            { model
                | cycles = 0
                , instructions = []
                , instructionOffsetMap = Dict.empty
                , memory = ( 0, [] )
                , registers = Registers.new
                , breakpoints = Set.empty
            }
    in
        ( newModel, cmd )


scrollElementIntoView : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
scrollElementIntoView class appInput =
    appInput
        |> \( inputMessage, inputCmd ) ->
            ( inputMessage, Cmd.batch [ inputCmd, Ports.scrollElementIntoViewCommand class ] )


handleInstructionsResponse : List Instruction.Instruction -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleInstructionsResponse instructions appInput =
    let
        ( inputModel, inputCmd ) =
            appInput

        instrMap =
            instructions
                |> List.indexedMap (\index instr -> ( Instruction.getOffset instr, index ))
                |> Dict.fromList
    in
        ( { inputModel | instructions = instructions, instructionOffsetMap = instrMap }, inputCmd )


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
                |> transitionDebuggerState DebuggerState.Pause
                |> fetchInstructionsIfNeeded
                |> handleBreakCondition reason snapshot
                |> \( outputModel, outputCmd ) -> ( applySnapshot outputModel snapshot, outputCmd )

        Crash reason snapshot ->
            appInput
                |> transitionDebuggerState DebuggerState.Pause
                |> \( outputModel, outputCmd ) ->
                    ( applySnapshot outputModel snapshot, outputCmd )
                        |> consoleMessage ("A crash has occurred: " ++ (crashReasonToString reason))


applySnapshot : Model -> CpuSnapshot.CpuSnapshot -> Model
applySnapshot model snapshot =
    { model
        | registers = snapshot.registers
        , cycles = snapshot.cycles
        , memory = snapshot.memory
    }


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

        DebuggerCommand.Trap ->
            appInput
                |> consoleMessage ("Trap detected @ 0x" ++ toHex snapshot.registers.pc)

        _ ->
            appInput


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.onOpen DebuggerConnectionOpened
        , WebSocket.onClose DebuggerConnectionClosed
        , WebSocket.listen wsDebuggerEndpoint <|
            DebuggerCommand.decode model.memory DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ id Styles.Container ]
        [ header []
            [ div [ id Styles.HeaderControls ]
                [ Registers.view model
                , div [ id Styles.DebuggerButtons ]
                    [ button [ class [ Styles.Button ], onClick ContinueClick, title "Continue" ] [ Icons.continue ]
                    , button [ class [ Styles.Button ], onClick StepClick, title "Step" ] [ Icons.step ]
                    , button [ class [ Styles.Button ], onClick ScrollInstructionIntoViewClick, title "Find Current Instruction" ] [ Icons.magnifyingGlass ]
                    ]
                , Byte.toggleDisplayView UpdateByteFormat model
                ]
            , div [ id Styles.StatusStrip, stripStyles model ] []
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


stripStyles : Model -> Attribute msg
stripStyles model =
    classList
        [ ( Styles.DebuggerConnected, model.debuggerState /= DebuggerState.NotConnected )
        , ( Styles.DebuggerNotConnected, model.debuggerState == DebuggerState.NotConnected )
        ]


styles : List Css.Snippet
styles =
    [ Styles.id Styles.Container
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.height (Css.vh 100)
        , Css.children
            [ Css.Elements.header
                [ Css.property "flex" "0 1 auto"
                , Css.backgroundColor Colors.headerColor
                , Css.boxShadow5 (Css.px 0) (Css.px 2) (Css.px 2) (Css.px -2) (Css.rgba 0 0 0 0.4)
                , Css.children
                    [ Styles.id Styles.HeaderControls
                        [ Css.padding (Css.px 5)
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
                    , Styles.id Styles.StatusStrip
                        [ Css.width (Css.pct 100)
                        , Css.height (Css.px 2)
                        ]
                    , Styles.class Styles.DebuggerConnected
                        [ Css.backgroundColor Colors.statusStripConnectedColor
                        ]
                    , Styles.class Styles.DebuggerNotConnected
                        [ Css.backgroundColor Colors.statusStripDisconnectedColor
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
        [ Css.backgroundColor Colors.consoleBackground
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
        [ Css.verticalAlign Css.top
        , Css.children
            [ Css.everything
                [ Css.marginLeft (Css.px 5)
                , Css.first
                    [ Css.marginLeft (Css.px 0)
                    ]
                ]
            ]
        ]
    ]
