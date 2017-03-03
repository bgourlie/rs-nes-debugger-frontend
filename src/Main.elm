module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button, header, input, fieldset)
import Html.Attributes exposing (disabled, checked, type_, title)
import Html.Events exposing (onClick)
import Dom
import Set exposing (Set)
import WebSocket
import Json.Decode as Json
import Css
import Keyboard
import ParseInt exposing (toHex)
import ConsoleCommand
import DebuggerState
import DebuggerCommand exposing (BreakReason, CrashReason, DebuggerCommand(..), crashReasonToString)
import Instruction
import Ports
import Continue
import Registers
import Step
import AppState
import Console
import Colors
import Byte
import ByteArray
import Breakpoints
import HexEditor
import Task
import Instruction
import Styles
import Memory
import ToggleBreakpoint
import ToggleNmiBreakpoint


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
    { appState : AppState.AppState
    , messages : List ( String, Int )
    , consoleInput : String
    , cycles : Int
    , instructionsDisplayed : Int
    , memory : Memory.Memory
    , memoryViewOffset : Int
    , registers : Registers.Registers
    , showConsoleInput : Bool
    , breakpoints : Breakpoints.Breakpoints
    , memoryByteFormat : Byte.Format
    , registersByteFormat : Byte.Format
    , offsetByteFormat : Byte.Format
    , operandByteFormat : Byte.Format
    , screen : DebuggerState.Screen
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { appState = AppState.NotConnected
            , messages = [ ( "Welcome to the rs-nes debugger!", 0 ) ]
            , cycles = 0
            , consoleInput = ""
            , instructionsDisplayed = 512
            , memory = ( 0, ByteArray.empty )
            , memoryViewOffset = 0
            , registers = Registers.new
            , breakpoints = Set.empty
            , showConsoleInput = False
            , memoryByteFormat = Byte.Hex
            , registersByteFormat = Byte.Hex
            , offsetByteFormat = Byte.Hex
            , operandByteFormat = Byte.Hex
            , screen = { width = 0, height = 0, imgData = "" }
            }
    in
        ( model, Cmd.none )



-- UPDATE


type Msg
    = DebuggerConnectionOpened String
    | DebuggerConnectionClosed String
    | DebuggerCommandReceived DebuggerCommand.ReceiveResult
    | ToggleBreakpoint Int
    | ToggleBreakpointResult ToggleBreakpoint.Result
    | ToggleNmiBreakpoint
    | ToggleNmiBreakpointResult ToggleNmiBreakpoint.Result
    | Step
    | StepResult Step.Result
    | Continue
    | ContinueResult Continue.Result
    | ScrollInstructionIntoView
    | UpdateMemoryByteFormat Byte.Format
    | UpdateConsoleInput String
    | SubmitConsoleCommand
    | ShowConsoleInput Bool
    | KeyPressed Int
    | UnknownState ( AppState.AppState, AppState.Input )
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebuggerConnectionOpened name ->
            ( model, Cmd.none )
                |> transitionAppState AppState.Connect
                |> clearCpuState
                |> consoleMessage ("Connected to debugger at " ++ name)

        DebuggerConnectionClosed _ ->
            ( model, Cmd.none )
                |> transitionAppState AppState.Disconnect
                |> consoleMessage "Disconnected from debugger"

        DebuggerCommandReceived result ->
            case result of
                DebuggerCommand.Success debuggerCommand ->
                    ( model, Cmd.none )
                        |> handleDebuggerCommand debuggerCommand

                DebuggerCommand.Error msg ->
                    ( model, Cmd.none )
                        |> consoleMessage ("Unable to receive debugger command: " ++ msg)

        ToggleBreakpoint address ->
            ( model, ToggleBreakpoint.request address ToggleBreakpointResult )

        ToggleBreakpointResult resp ->
            case resp of
                ToggleBreakpoint.Success { isSet, offset } ->
                    let
                        message =
                            if isSet then
                                "Breakpoint set @ 0x" ++ (toHex offset)
                            else
                                "Breakpoint unset @ 0x" ++ (toHex offset)
                    in
                        ( { model | breakpoints = (Breakpoints.toggleBreakpoint model isSet offset) }, Cmd.none )
                            |> consoleMessage message

                ToggleBreakpoint.Error msg ->
                    consoleMessage ("Set breakpoint fail: " ++ msg) ( model, Cmd.none )

        ToggleNmiBreakpoint ->
            ( model, ToggleNmiBreakpoint.request ToggleNmiBreakpointResult )

        ToggleNmiBreakpointResult resp ->
            case resp of
                ToggleNmiBreakpoint.Success { isSet } ->
                    let
                        message =
                            if isSet then
                                "Break-on-NMI set"
                            else
                                "Break-on-NMI unset"
                    in
                        ( model, Cmd.none )
                            |> consoleMessage message

                ToggleNmiBreakpoint.Error msg ->
                    consoleMessage ("Break-on-NMI toggle fail: " ++ msg) ( model, Cmd.none )

        Step ->
            ( model, Cmd.none )
                |> transitionAppState AppState.Step

        StepResult resp ->
            case resp of
                Step.Success _ ->
                    ( model, Cmd.none )
                        |> transitionAppState (AppState.StepRequestComplete AppState.Success)

                Step.Error _ ->
                    ( model, Cmd.none )
                        |> transitionAppState (AppState.StepRequestComplete AppState.Fail)
                        |> consoleMessage "Step request failed"

        Continue ->
            ( model, Cmd.none )
                |> transitionAppState AppState.Continue
                |> consoleMessage "Continuing execution..."

        ContinueResult resp ->
            case resp of
                Continue.Success _ ->
                    ( model, Cmd.none )
                        |> transitionAppState (AppState.ContinueRequestComplete AppState.Success)

                Continue.Error msg ->
                    ( model, Cmd.none )
                        |> transitionAppState (AppState.ContinueRequestComplete AppState.Fail)
                        |> consoleMessage ("Continue request fail: " ++ msg)

        ScrollInstructionIntoView ->
            ( model, Cmd.none )
                |> scrollElementIntoView (toString Styles.CurrentInstruction)

        UpdateMemoryByteFormat byteFormat ->
            ( { model | memoryByteFormat = byteFormat }, Cmd.none )

        UpdateConsoleInput input ->
            ( { model | consoleInput = input }, Cmd.none )

        SubmitConsoleCommand ->
            ( model, Cmd.none )
                |> executeConsoleCommand

        KeyPressed keyCode ->
            ( model, Cmd.none )
                |> handleKeyPress keyCode

        ShowConsoleInput shouldShow ->
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
                    update (ShowConsoleInput True) model
            in
                ( newModel, Cmd.batch [ cmd, newCmd ] )

        83 ->
            -- "s" for step
            let
                ( newModel, newCmd ) =
                    update Step model
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
                    update Continue model
            in
                ( newModel, Cmd.batch [ cmd, newCmd ] )

        _ ->
            ( model, cmd )


executeConsoleCommand : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
executeConsoleCommand ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            case model.consoleInput of
                "" ->
                    ( model, cmd )

                _ ->
                    case ConsoleCommand.parse model.consoleInput of
                        Ok consoleCommand ->
                            case consoleCommand of
                                ConsoleCommand.SetOffsetByteView byteFormat ->
                                    ( { model | offsetByteFormat = byteFormat }, cmd )
                                        |> consoleMessage ("Updated offset byte format to " ++ (toString byteFormat))

                                ConsoleCommand.SetMemoryByteView byteFormat ->
                                    ( { model | memoryByteFormat = byteFormat }, cmd )
                                        |> consoleMessage ("Updated memory byte format to " ++ (toString byteFormat))

                                ConsoleCommand.SetOperandByteView byteFormat ->
                                    ( { model | operandByteFormat = byteFormat }, cmd )
                                        |> consoleMessage ("Updated operand byte format to " ++ (toString byteFormat))

                                ConsoleCommand.SetRegistersByteView byteFormat ->
                                    ( { model | registersByteFormat = byteFormat }, cmd )
                                        |> consoleMessage ("Updated registers byte format to " ++ (toString byteFormat))

                                ConsoleCommand.ToggleBreakpoint bpType ->
                                    case bpType of
                                        ConsoleCommand.Offset offset ->
                                            update (ToggleBreakpoint offset) model

                                        ConsoleCommand.Nmi ->
                                            update ToggleNmiBreakpoint model

                                ConsoleCommand.JumpToMemory offset ->
                                    updateMemoryViewOffset offset ( model, cmd )

                        Err _ ->
                            ( model, cmd )
                                |> consoleMessage ("Unknown console command: " ++ model.consoleInput)

        ( finalModel, showConsoleInputCmd ) =
            update (ShowConsoleInput False) newModel
    in
        ( { finalModel | consoleInput = "" }, Cmd.batch [ newCmd, showConsoleInputCmd ] )


updateMemoryViewOffset : Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateMemoryViewOffset offset ( model, cmd ) =
    if offset >= 0 && offset <= 0xFFFF then
        ( { model | memoryViewOffset = offset }, cmd )
            |> consoleMessage ("Displaying memory starting at offset 0x" ++ (toHex offset))
    else
        consoleMessage "Invalid offset specified" ( model, cmd )


transitionAppState : AppState.Input -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
transitionAppState smInput appInput =
    AppState.transition StepResult ContinueResult UnknownState smInput appInput


clearCpuState : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
clearCpuState appInput =
    let
        ( model, cmd ) =
            appInput

        newModel =
            { model
                | cycles = 0
                , memory = ( 0, ByteArray.empty )
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


onBreakpoint : Model -> Bool
onBreakpoint model =
    Set.member model.registers.pc model.breakpoints


consoleMessage : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
consoleMessage message appInput =
    Console.addMessage NoOp message appInput


handleDebuggerCommand : DebuggerCommand -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleDebuggerCommand debuggerCommand appInput =
    case debuggerCommand of
        Break reason snapshot ->
            appInput
                |> transitionAppState AppState.Pause
                |> handleBreakCondition reason snapshot
                |> \( outputModel, outputCmd ) -> ( applySnapshot outputModel snapshot, outputCmd )

        Crash reason snapshot ->
            appInput
                |> transitionAppState AppState.Pause
                |> \( outputModel, outputCmd ) ->
                    ( applySnapshot outputModel snapshot, outputCmd )
                        |> consoleMessage ("A crash has occurred: " ++ (crashReasonToString reason))


applySnapshot : Model -> DebuggerState.State -> Model
applySnapshot model snapshot =
    { model
        | registers = snapshot.registers
        , cycles = snapshot.cycles
        , memory = snapshot.memory
        , screen = snapshot.screen
    }


handleBreakCondition : BreakReason -> DebuggerState.State -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleBreakCondition breakReason snapshot appInput =
    case breakReason of
        DebuggerCommand.Breakpoint ->
            appInput
                |> consoleMessage ("Hit breakpoint @ 0x" ++ toHex snapshot.registers.pc)

        DebuggerCommand.Trap ->
            appInput
                |> consoleMessage ("Trap detected @ 0x" ++ toHex snapshot.registers.pc)

        DebuggerCommand.Nmi ->
            appInput
                |> consoleMessage "Breaking on NMI"

        _ ->
            appInput


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups (\keyCode -> KeyPressed keyCode)
        , WebSocket.onOpen DebuggerConnectionOpened
        , WebSocket.onClose DebuggerConnectionClosed
        , WebSocket.listen wsDebuggerEndpoint <|
            DebuggerCommand.decode model.memory DebuggerCommandReceived
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ id Styles.Container ]
        [ div
            [ id Styles.StatusStrip
            , classList
                [ ( Styles.DebuggerConnected, model.appState /= AppState.NotConnected )
                , ( Styles.DebuggerNotConnected, model.appState == AppState.NotConnected )
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
            , Html.Events.onBlur (ShowConsoleInput False)
            , Html.Attributes.value model.consoleInput
            , handleInput
            ]
            []
        , div [ id Styles.ScreenContainer ] [ screen model ]
        ]


screen : Model -> Html Msg
screen { screen } =
    case screen.imgData of
        "" ->
            div [ id Styles.NoScreen ] [ text "No screen data provided" ]

        _ ->
            Html.img
                [ id Styles.Screen
                , Html.Attributes.src ("data:image/png;base64," ++ screen.imgData)
                ]
                []


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
                        ShowConsoleInput False

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
    , Styles.id Styles.ScreenContainer
        [ Css.position Css.fixed
        , Css.right (Css.px 0)
        , Css.top (Css.px 0)
        ]
    ]
