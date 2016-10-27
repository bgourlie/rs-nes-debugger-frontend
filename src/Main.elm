port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button)
import Html.Events exposing (onClick)
import Html.App as App
import Http
import Task exposing (Task)
import List exposing (map, map2)
import WebSocket
import CpuSnapshot exposing (CpuSnapshot, cpuSnapshotDecoder)
import ParseInt exposing (toHex)
import Styles
import DebuggerCommand
import DebuggerCommand exposing (DebuggerCommand(Break))
import Json.Decode
import ToggleBreakpoint


{ id, class, classList } =
    Styles.helpers


wsDebuggerEndpoint =
    "ws://localhost:9976"


memoryEndpoint =
    "http://localhost:9975/memory"


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
    = DebuggerCommandReceived DebuggerCommand
    | MalformedDebuggerCommand String
    | SetBreakpointClick Int
    | SetBreakpointSuccess ToggleBreakpoint.Response
    | SetBreakpointFail Http.Error
    | FetchCpuSnapshotSuccess CpuSnapshot
    | FetchCpuSnapshotFail Http.Error
    | Decoded (List String)
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        DebuggerCommandReceived cmd ->
            ( model, getCpuSnapshot )

        MalformedDebuggerCommand msg ->
            ( { model | message = "Malformed debugger command" ++ msg }, Cmd.none )

        Decoded bytes ->
            ( { model | message = "DECODED!", decodedRom = bytes }, Cmd.none )

        SetBreakpointClick address ->
            ( model, toggleBreakpoint address )

        SetBreakpointSuccess resp ->
            ( { model | message = "Breakpoint set at " ++ toString resp.address }, Cmd.none )

        SetBreakpointFail err ->
            ( { model | message = "Set breakpoint fail: " ++ toString err }, Cmd.none )

        FetchCpuSnapshotSuccess cpuSnapshot ->
            ( { model | message = "Decoding...", programCounter = cpuSnapshot.registers.pc }, decode ( cpuSnapshot.memory, decodeStartRange cpuSnapshot.registers.pc, decodeEndRange cpuSnapshot.registers.pc + 20 ) )

        FetchCpuSnapshotFail err ->
            ( { model | message = "Fetch Fail: " ++ toString err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ decoded (\asm -> Decoded asm)
        , WebSocket.listen wsDebuggerEndpoint decodeDebuggerCommand
        ]


decodeDebuggerCommand : String -> AppMessage
decodeDebuggerCommand json =
    case DebuggerCommand.fromJson json of
        Ok cmd ->
            DebuggerCommandReceived cmd

        Err msg ->
            MalformedDebuggerCommand msg


getCpuSnapshot : Cmd AppMessage
getCpuSnapshot =
    Task.perform FetchCpuSnapshotFail FetchCpuSnapshotSuccess (Http.get cpuSnapshotDecoder memoryEndpoint)


toggleBreakpoint : Int -> Cmd AppMessage
toggleBreakpoint address =
    let
        decoder =
            ToggleBreakpoint.responseDecoder

        endpoint =
            ToggleBreakpoint.endpoint address
    in
        Task.perform SetBreakpointFail SetBreakpointSuccess (Http.get decoder endpoint)


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
        [ button [ onClick <| SetBreakpointClick 1232 ] [ text "Set breakpoint" ]
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
