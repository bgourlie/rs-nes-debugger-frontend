port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button)
import Html.Events exposing (onClick)
import Html.App as App
import Http
import Task exposing (Task)
import List exposing (map, map2)
import WebSocket
import ParseInt exposing (toHex)
import Styles
import Json.Decode
import DebuggerCommand exposing (DebuggerCommand(Break))
import CpuSnapshot
import ToggleBreakpoint
import Continue
import Registers
import Step


{ id, class, classList } =
    Styles.helpers


wsDebuggerEndpoint =
    "ws://localhost:9976"


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { message : String
    , decodedRom : List String
    , registers : Registers.Model
    }



-- port for sending decode requests out to JavaScript


port decode : ( List Int, Int, Int ) -> Cmd msg



-- port for listening for decoded instruction from JavaScript


port decoded : (List String -> msg) -> Sub msg


init : ( Model, Cmd AppMessage )
init =
    ( { message = "Hello!", decodedRom = [], registers = Registers.new }, Cmd.none )



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
    | CpuSnapshotRequestSuccess CpuSnapshot.Model
    | CpuSnapshotRequestFail Http.Error
    | Decoded (List String)
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        DebuggerCommandReceiveSuccess cmd ->
            ( model, CpuSnapshot.request CpuSnapshotRequestFail CpuSnapshotRequestSuccess )

        DebuggerCommandReceiveFail msg ->
            ( { model | message = "Unable to receive debugger command: " ++ msg }, Cmd.none )

        Decoded bytes ->
            ( { model | message = "DECODED!", decodedRom = bytes }, Cmd.none )

        SetBreakpointClick address ->
            ( model, ToggleBreakpoint.request address SetBreakpointRequestFail SetBreakpointRequestSuccess )

        SetBreakpointRequestSuccess resp ->
            ( { model | message = "Breakpoint set at " ++ toString resp.address }, Cmd.none )

        SetBreakpointRequestFail err ->
            ( { model | message = "Set breakpoint fail: " ++ toString err }, Cmd.none )

        StepClick ->
            ( model, Step.request StepRequestFail StepRequestSuccess )

        StepRequestSuccess resp ->
            ( { model | message = "Stepped!" }, Cmd.none )

        StepRequestFail err ->
            ( { model | message = "Step request fail: " ++ toString err }, Cmd.none )

        ContinueClick ->
            ( model, Continue.request ContinueRequestFail ContinueRequestSuccess )

        ContinueRequestSuccess resp ->
            ( { model | message = "Continued!" }, Cmd.none )

        ContinueRequestFail err ->
            ( { model | message = "Continue request fail: " ++ toString err }, Cmd.none )

        CpuSnapshotRequestSuccess cpuSnapshot ->
            ( { model | message = "Decoding...", registers = cpuSnapshot.registers }, decode ( cpuSnapshot.memory, decodeStartRange cpuSnapshot.registers.pc, decodeEndRange cpuSnapshot.registers.pc + 20 ) )

        CpuSnapshotRequestFail err ->
            ( { model | message = "Fetch Fail: " ++ toString err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ decoded (\asm -> Decoded asm)
        , WebSocket.listen wsDebuggerEndpoint <| DebuggerCommand.decode DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
        ]


decodeStartRange : Int -> Int
decodeStartRange pc =
    if pc < 20 then
        0
    else
        pc - 20


decodeEndRange : Int -> Int
decodeEndRange pc =
    pc + 20



-- VIEW


view : Model -> Html AppMessage
view model =
    div []
        [ button [ onClick <| SetBreakpointClick 0x0418 ] [ text "Set breakpoint" ]
        , button [ onClick StepClick ] [ text "Step" ]
        , button [ onClick ContinueClick ] [ text "Continue" ]
        , div [] [ text model.message ]
        , div [] [ Registers.view model.registers ]
        , div [ id Styles.Instructions ] [ instructionList model ]
        ]


instructionList : Model -> Html AppMessage
instructionList model =
    let
        pc =
            model.registers.pc
    in
        ul []
            (map
                (\( str, addr ) ->
                    li [ instructionClass addr model ]
                        [ div [] [ text <| "0x" ++ toHex addr ]
                        , div [] [ text str ]
                        ]
                )
                (map2 (,) model.decodedRom [decodeStartRange pc..decodeEndRange pc])
            )


instructionClass address model =
    if address == model.registers.pc then
        class [ Styles.CurrentInstruction ]
    else
        class []
