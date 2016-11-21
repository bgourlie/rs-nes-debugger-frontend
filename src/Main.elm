port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button, header, input)
import Html.Attributes exposing (disabled, checked, type_)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Http
import WebSocket
import Css exposing ((#))
import Css.Elements
import ParseInt exposing (toHex)
import CssCommon
import DebuggerCommand exposing (DebuggerCommand(Break))
import Instruction
import ToggleBreakpoint
import Continue
import Registers
import Step
import Colors


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
    { messages : List String
    , cycles : Int
    , instructions : List Instruction.Model
    , registers : Registers.Model
    , stepState : StepState
    , breakpoints : Set Int
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
            { messages = [ "Welcome to the rs-nes debugger!" ]
            , cycles = 0
            , instructions = []
            , registers = Registers.new
            , stepState = Off
            , breakpoints = Set.empty
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
                newModel =
                    addMessage model ("Unable to receive debugger command: " ++ msg)
            in
                ( newModel, Cmd.none )

        ToggleBreakpointClick address ->
            ( model, ToggleBreakpoint.request address ToggleBreakpointRequestFail ToggleBreakpointRequestSuccess )

        ToggleBreakpointRequestSuccess resp ->
            let
                newModel =
                    addMessage model ("Breakpoint set @ 0x" ++ toString resp.offset)
            in
                ( { newModel | breakpoints = (updateBreakpoints model.breakpoints resp) }, Cmd.none )

        ToggleBreakpointRequestFail err ->
            let
                newModel =
                    addMessage model ("Set breakpoint fail: " ++ toString err)
            in
                ( newModel, Cmd.none )

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
            in
                ( handleStepModel, Cmd.batch [ handleStepCmd, Continue.request ContinueRequestFail ContinueRequestSuccess ] )

        ContinueRequestSuccess resp ->
            ( model, Cmd.none )

        ContinueRequestFail err ->
            let
                newModel =
                    addMessage model ("Continue request fail: " ++ toString err)
            in
                ( newModel, Cmd.none )

        ScrollInstructionIntoView ->
            ( model, scrollElementIntoView <| toString Instruction.CurrentInstruction )

        NoOp ->
            ( model, Cmd.none )


addMessage : Model -> String -> Model
addMessage model message =
    { model | messages = message :: model.messages }


updateBreakpoints : Set Int -> ToggleBreakpoint.Model -> Set Int
updateBreakpoints breakpoints toggleBreakpointResponse =
    if toggleBreakpointResponse.isSet then
        Set.insert toggleBreakpointResponse.offset breakpoints
    else
        Set.remove toggleBreakpointResponse.offset breakpoints


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
                    handleBreakCondition model reason
            in
                ( { newModel | instructions = snapshot.instructions, registers = snapshot.registers, cycles = snapshot.cycles }, cmd )


handleBreakCondition : Model -> DebuggerCommand.BreakReason -> ( Model, Cmd AppMessage )
handleBreakCondition model breakReason =
    case breakReason of
        DebuggerCommand.Breakpoint ->
            let
                ( postStepModel, postStepCmd ) =
                    handleStepInput AutoStepOff model
            in
                ( addMessage postStepModel ("Breakpoint hit @ 0x" ++ toHex model.registers.pc), postStepCmd )

        DebuggerCommand.Trap ->
            let
                ( postStepModel, postStepCmd ) =
                    handleStepInput AutoStepOff model
            in
                ( addMessage postStepModel ("Trap detected @ 0x" ++ toHex model.registers.pc), postStepCmd )

        _ ->
            ( model, Cmd.none )


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
            [ Registers.view model.registers
            , div [ id DebuggerButtons ]
                [ button [ onClick StepClick, disabled <| autoStepEnabled model ] [ text "Step" ]
                , input [ type_ "checkbox", checked <| autoStepEnabled model, onClick ToggleAutoStepClicked ] []
                , button [ onClick ContinueClick ] [ text "Continue" ]
                , button [ onClick ScrollInstructionIntoView ] [ text "Locate Current Instruction" ]
                ]
            , div [] [ text <| "Cycles: " ++ toString model.cycles ]
            ]
        , div [ id TwoColumn ]
            [ div [ id InstructionsViewContainer ]
                [ Instruction.view (\address -> ToggleBreakpointClick address) model.breakpoints model.registers.pc model.instructions
                ]
            , div [ id ConsoleContainer ]
                [ ul [ id Console, class [ CssCommon.List ] ] (List.map (\msg -> li [] [ text msg ]) (List.reverse model.messages))
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
                , Css.borderBottom3 (Css.px 1) (Css.solid) Colors.headerBorder
                , Css.padding (Css.px 5)
                , Css.boxShadow5 (Css.px 0) (Css.px 2) (Css.px 2) (Css.px -2) (Css.rgba 0 0 0 0.4)
                , Css.children
                    [ Css.Elements.div
                        [ Css.display Css.inlineBlock
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
    , (#) Console
        [ Css.height (Css.pct 100)
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
