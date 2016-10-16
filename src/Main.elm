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
    = DebuggerMessage String
    | FetchCpuSnapshot
    | FetchCpuSnapshotSuccess CpuSnapshot
    | FetchCpuSnapshotFail Http.Error
    | Decoded (List String)
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        DebuggerMessage str ->
            ( model, Cmd.none )

        Decoded bytes ->
            ( { model | message = "DECODED!", decodedRom = bytes }, Cmd.none )

        FetchCpuSnapshot ->
            ( { model | message = "Fetching..." }, getCpuSnapshot )

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
        , WebSocket.listen wsDebuggerEndpoint DebuggerMessage
        ]


getCpuSnapshot : Cmd AppMessage
getCpuSnapshot =
    Task.perform FetchCpuSnapshotFail FetchCpuSnapshotSuccess (Http.get cpuSnapshotDecoder memoryEndpoint)


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
        [ button [ onClick FetchCpuSnapshot ] [ text "Get Snapshot" ]
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
