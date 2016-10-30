port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button)
import Html.Events exposing (onClick)
import Html.App as App
import Http
import Task exposing (Task)
import List exposing (map, map2)
import WebSocket
import CpuSnapshot
import ParseInt exposing (toHex)
import Styles
import DebuggerCommand
import DebuggerCommand exposing (DebuggerCommand(Break))
import Json.Decode
import ToggleBreakpoint
import Continue


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
    , programCounter : Int
    }



-- port for sending decode requests out to JavaScript


port decode : ( List Int, Int, Int ) -> Cmd msg



-- port for listening for decoded instruction from JavaScript


port decoded : (List String -> msg) -> Sub msg


init : ( Model, Cmd AppMessage )
init =
    ( { message = "Hello!", decodedRom = [], programCounter = 0 }, Cmd.none )



-- UPDATE


type AppMessage
    = DebuggerCommandReceiveSuccess DebuggerCommand
    | DebuggerCommandReceiveFail String
    | SetBreakpointClick Int
    | SetBreakpointRequestSuccess ToggleBreakpoint.Response
    | SetBreakpointRequestFail Http.Error
    | ContinueClick
    | ContinueRequestSuccess Continue.Response
    | ContinueRequestFail Http.Error
    | CpuSnapshotRequestSuccess CpuSnapshot.Response
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

        ContinueClick ->
            ( model, Continue.request ContinueRequestFail ContinueRequestSuccess )

        ContinueRequestSuccess resp ->
            ( { model | message = "Continued!" }, Cmd.none )

        ContinueRequestFail err ->
            ( { model | message = "Continue request fail: " ++ toString err }, Cmd.none )

        CpuSnapshotRequestSuccess cpuSnapshot ->
            ( { model | message = "Decoding...", programCounter = cpuSnapshot.registers.pc }, decode ( cpuSnapshot.memory, decodeStartRange cpuSnapshot.registers.pc, decodeEndRange cpuSnapshot.registers.pc + 20 ) )

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
        [ button [ onClick <| SetBreakpointClick 0x3607 ] [ text "Set breakpoint" ]
        , button [ onClick ContinueClick ] [ text "Continue" ]
        , div [] [ text model.message ]
        , div [ id Styles.Instructions ] [ instructionList model ]
        ]


instructionList : Model -> Html AppMessage
instructionList model =
    ul []
        (map
            (\( str, addr ) ->
                li [ instructionClass addr model ]
                    [ div [] [ text <| "0x" ++ toHex addr ]
                    , div [] [ text str ]
                    ]
            )
            (map2 (,) model.decodedRom [decodeStartRange model.programCounter..decodeEndRange model.programCounter])
        )


instructionClass addr model =
    if addr == model.programCounter then
        class [ Styles.CurrentInstruction ]
    else
        class []
