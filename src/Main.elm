module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button, header, input, fieldset)
import Html.Attributes exposing (disabled, checked, type_, title)
import Html.Events exposing (onClick)
import Dom
import Set exposing (Set)
import Dict exposing (Dict)
import Http
import WebSocket
import Json.Decode as Json
import Css
import Css.Elements
import Keyboard
import ParseInt exposing (toHex)
import ConsoleCommand
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
import Task
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
    , consoleInput : String
    , cycles : Int
    , instructions : List Instruction.Instruction
    , instructionsDisplayed : Int
    , instructionOffsetMap : Instruction.OffsetMap
    , instructionPivot : Int
    , memory : MemorySnapshot.MemorySnapshot
    , memoryViewOffset : Int
    , registers : Registers.Registers
    , showConsoleInput : Bool
    , breakpoints : Breakpoints.Breakpoints
    , memoryByteFormat : Byte.Format
    , registersByteFormat : Byte.Format
    , offsetByteFormat : Byte.Format
    , operandByteFormat : Byte.Format
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { debuggerState = DebuggerState.NotConnected
            , messages = [ ( "Welcome to the rs-nes debugger!", 0 ) ]
            , cycles = 0
            , consoleInput = ""
            , instructions = []
            , instructionsDisplayed = 512
            , instructionOffsetMap = Dict.empty
            , instructionPivot = 0
            , memory = ( 0, [] )
            , memoryViewOffset = 0
            , registers = Registers.new
            , breakpoints = Set.empty
            , showConsoleInput = False
            , memoryByteFormat = Byte.Hex
            , registersByteFormat = Byte.Hex
            , offsetByteFormat = Byte.Hex
            , operandByteFormat = Byte.Hex
            }
    in
        ( model, Cmd.none )



-- UPDATE


type Msg
    = DebuggerConnectionOpened String
    | DebuggerConnectionClosed String
    | DebuggerCommandReceiveSuccess DebuggerCommand
    | DebuggerCommandReceiveFail String
    | ToggleBreakpoint Int
    | ToggleBreakpointRequestSuccess ToggleBreakpoint.Message
    | ToggleBreakpointRequestFail Http.Error
    | StepClick
    | StepRequestSuccess Step.Model
    | StepRequestFail Http.Error
    | ContinueClick
    | ContinueRequestSuccess Continue.Model
    | ContinueRequestFail Http.Error
    | ScrollInstructionIntoView
    | ScrollConsoleFail Dom.Error
    | ScrollConsoleSucceed
    | UpdateMemoryByteFormat Byte.Format
    | InstructionRequestSuccess (List Instruction.Instruction)
    | InstructionRequestFail Http.Error
    | UpdateConsoleInput String
    | SubmitConsoleCommand
    | SetDisplayConsoleInput Bool
    | KeyPressed Int
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

        ToggleBreakpoint address ->
            ( model, ToggleBreakpoint.request address ToggleBreakpointRequestFail ToggleBreakpointRequestSuccess )

        ToggleBreakpointRequestSuccess resp ->
            let
                message =
                    if resp.isSet then
                        "Breakpoint set @ 0x" ++ (toHex resp.offset)
                    else
                        "Breakpoint unset @ 0x" ++ (toHex resp.offset)
            in
                ( { model | breakpoints = (Breakpoints.toggle model resp.isSet resp.offset) }, Cmd.none )
                    |> consoleMessage message

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

        ScrollInstructionIntoView ->
            ( { model | instructionPivot = model.registers.pc }, Cmd.none )
                |> scrollElementIntoView (toString Styles.CurrentInstruction)

        ScrollConsoleSucceed ->
            ( model, Cmd.none )

        ScrollConsoleFail _ ->
            ( model, Cmd.none )

        UpdateMemoryByteFormat byteFormat ->
            ( { model | memoryByteFormat = byteFormat }, Cmd.none )

        InstructionRequestSuccess instructions ->
            ( model, Cmd.none )
                |> consoleMessage ("Received " ++ (toString <| List.length instructions) ++ " instructions")
                |> handleInstructionsResponse instructions

        InstructionRequestFail err ->
            ( model, Cmd.none )
                |> consoleMessage ("Continue request fail: " ++ toString err)

        UpdateConsoleInput input ->
            ( { model | consoleInput = input }, Cmd.none )

        SubmitConsoleCommand ->
            ( model, Cmd.none )
                |> executeConsoleCommand

        KeyPressed keyCode ->
            ( model, Cmd.none )
                |> handleKeyPress keyCode

        SetDisplayConsoleInput shouldShow ->
            let
                task =
                    if shouldShow then
                        Dom.focus (toString Styles.ConsoleInput)
                    else
                        Dom.blur (toString Styles.ConsoleInput)
            in
                ( { model | showConsoleInput = shouldShow }, Task.attempt (\_ -> NoOp) task )

        UnknownState ( state, input ) ->
            ( model, Cmd.none )
                |> consoleMessage ("Uh oh, the following state transition wasn't handled: " ++ (toString input) ++ " -> " ++ (toString state))

        NoOp ->
            ( model, Cmd.none )


handleKeyPress : Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleKeyPress keyCode ( model, cmd ) =
    case keyCode of
        191 ->
            -- "/" for displaying console input
            let
                ( newModel, newCmd ) =
                    update (SetDisplayConsoleInput True) model
            in
                ( newModel, Cmd.batch [ cmd, newCmd ] )

        83 ->
            -- "s" for step
            let
                ( newModel, newCmd ) =
                    update StepClick model
            in
                ( newModel, Cmd.batch [ cmd, newCmd ] )

        70 ->
            -- "f" for find current instruction
            let
                ( newModel, newCmd ) =
                    update ScrollInstructionIntoView model
            in
                ( newModel, Cmd.batch [ cmd, newCmd ] )

        67 ->
            -- "c" for continue
            let
                ( newModel, newCmd ) =
                    update ContinueClick model
            in
                ( newModel, Cmd.batch [ cmd, newCmd ] )

        _ ->
            ( model, cmd )


executeConsoleCommand : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
executeConsoleCommand ( model, cmd ) =
    case model.consoleInput of
        "" ->
            update (SetDisplayConsoleInput False) model

        _ ->
            let
                updatedModel =
                    { model | consoleInput = "" }
            in
                case ConsoleCommand.parse model.consoleInput of
                    Ok consoleCommand ->
                        case consoleCommand of
                            ConsoleCommand.SetOffsetByteView byteFormat ->
                                ( { updatedModel | offsetByteFormat = byteFormat }, cmd )
                                    |> consoleMessage ("Updated offset byte format to " ++ (toString byteFormat))

                            ConsoleCommand.SetMemoryByteView byteFormat ->
                                ( { updatedModel | memoryByteFormat = byteFormat }, cmd )
                                    |> consoleMessage ("Updated memory byte format to " ++ (toString byteFormat))

                            ConsoleCommand.SetOperandByteView byteFormat ->
                                ( { updatedModel | operandByteFormat = byteFormat }, cmd )
                                    |> consoleMessage ("Updated operand byte format to " ++ (toString byteFormat))

                            ConsoleCommand.SetRegistersByteView byteFormat ->
                                ( { updatedModel | registersByteFormat = byteFormat }, cmd )
                                    |> consoleMessage ("Updated registers byte format to " ++ (toString byteFormat))

                            ConsoleCommand.ToggleBreakpoint offset ->
                                update (ToggleBreakpoint offset) updatedModel

                            ConsoleCommand.JumpToMemory offset ->
                                updateMemoryViewOffset offset ( updatedModel, cmd )

                    Err _ ->
                        ( updatedModel, cmd )
                            |> consoleMessage ("Unknown console command: " ++ model.consoleInput)


updateMemoryViewOffset : Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateMemoryViewOffset offset ( model, cmd ) =
    if offset >= 0 && offset <= 0xFFFF then
        ( { model | memoryViewOffset = offset }, cmd )
            |> consoleMessage ("Displaying memory starting at offset 0x" ++ (toHex offset))
    else
        consoleMessage "Invalid offset specified" ( model, cmd )


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
        , instructionPivot = snapshot.registers.pc
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
        [ Keyboard.ups (\keyCode -> KeyPressed keyCode)
        , WebSocket.onOpen DebuggerConnectionOpened
        , WebSocket.onClose DebuggerConnectionClosed
        , WebSocket.listen wsDebuggerEndpoint <|
            DebuggerCommand.decode model.memory DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ id Styles.Container ]
        [ div
            [ id Styles.StatusStrip
            , classList
                [ ( Styles.DebuggerConnected, model.debuggerState /= DebuggerState.NotConnected )
                , ( Styles.DebuggerNotConnected, model.debuggerState == DebuggerState.NotConnected )
                ]
            ]
            []
        , div [ id Styles.TwoColumn ]
            [ div [ id Styles.LeftColumn ]
                [ Registers.view model
                , div [ id Styles.InstructionsContainer ]
                    [ Instruction.view (\address -> ToggleBreakpoint address) model
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
        , input
            [ id Styles.ConsoleInput
            , classList [ ( Styles.ConsoleInputDisplayed, model.showConsoleInput ) ]
            , Html.Attributes.type_ "text"
            , Html.Events.onInput UpdateConsoleInput
            , Html.Events.onBlur (SetDisplayConsoleInput False)
            , Html.Attributes.value model.consoleInput
            , handleInput
            ]
            []
        ]


handleInput : Html.Attribute Msg
handleInput =
    Html.Events.onWithOptions "keyup"
        { stopPropagation = True, preventDefault = False }
        (Json.map
            (\keyCode ->
                case keyCode of
                    13 ->
                        SubmitConsoleCommand

                    27 ->
                        SetDisplayConsoleInput False

                    _ ->
                        NoOp
            )
            Html.Events.keyCode
        )


styles : List Css.Snippet
styles =
    [ Styles.id Styles.Container
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.height (Css.vh 100)
        , Css.children
            [ Styles.id Styles.StatusStrip
                [ Css.width (Css.pct 100)
                , Css.height (Css.px 3)
                , Styles.withClass Styles.DebuggerConnected
                    [ Css.backgroundColor Colors.statusStripConnectedColor
                    ]
                , Styles.withClass Styles.DebuggerNotConnected
                    [ Css.backgroundColor Colors.statusStripDisconnectedColor
                    ]
                ]
            , Styles.id Styles.TwoColumn
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.flexGrow (Css.num 1)
                , Css.property "height" "calc(100% - 3px)"
                , Css.children
                    [ Styles.id Styles.LeftColumn
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.flexGrow (Css.num 1)
                        , Css.flexBasis (Css.px 0)
                        , Css.overflowY Css.auto
                        , Css.children
                            [ Styles.id Styles.Registers
                                [ Css.flexGrow (Css.num 1)
                                , Css.flexBasis (Css.px 0)
                                ]
                            , Styles.id Styles.InstructionsContainer
                                [ Css.borderTop3 (Css.px 1) Css.solid Colors.headerBorder
                                , Css.overflowY Css.scroll
                                , Css.flexGrow (Css.num 1)
                                , Css.property "height" "calc(100% - 40px)"
                                ]
                            ]
                        ]
                    , Styles.id Styles.RightColumn
                        [ Css.displayFlex
                        , Css.flex3 (Css.num 2) (Css.num 0) (Css.num 0)
                        , Css.flexDirection Css.column
                        , Css.overflow Css.auto
                        , Css.children
                            [ Styles.id Styles.ConsoleContainer
                                [ Css.backgroundColor Colors.consoleBackground
                                , Css.displayFlex
                                , Css.flex3 (Css.num 1) (Css.num 0) (Css.num 0)
                                ]
                            , Styles.id Styles.HexEditorContainer
                                [ Css.displayFlex
                                , Css.flex3 (Css.num 2) (Css.num 0) (Css.num 0)
                                , Css.overflowY Css.auto
                                , Css.position Css.relative
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , Styles.id Styles.ConsoleInput
        [ Css.position Css.fixed
        , Css.display Css.block
        , Css.property "transition" "bottom 100ms ease-out"
        , Css.bottom (Css.em -2)
        , Css.left (Css.px 0)
        , Css.width (Css.em 40)
        , Css.height (Css.em 2)
        , Css.marginTop Css.auto
        , Css.outline Css.none
        , Css.border (Css.px 0)
        , Css.fontFamily Css.monospace
        , Css.backgroundColor Colors.consoleInputBackground
        , Css.color Colors.consoleInputText
        , Css.fontSize (Css.em 1)
        , Css.padding2 (Css.em 0.2) (Css.em 0.4)
        , Styles.withClass Styles.ConsoleInputDisplayed
            [ Css.bottom (Css.em 0)
            ]
        ]
    ]
