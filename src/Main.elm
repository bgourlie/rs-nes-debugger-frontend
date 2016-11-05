port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, button)
import Html.Events exposing (onClick)
import Html.App as App
import Http
import WebSocket
import Css exposing ((#))
import Css.Elements
import CssCommon
import DebuggerCommand exposing (DebuggerCommand(Break))
import CpuSnapshot
import Instructions
import ToggleBreakpoint
import Continue
import Registers
import Step


{ id, class, classList } =
    CssCommon.helpers


wsDebuggerEndpoint : String
wsDebuggerEndpoint =
    "ws://localhost:9976"


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { messages : List String
    , decodedRom : List String
    , registers : Registers.Model
    }



-- port for sending decode requests out to JavaScript


port decode : ( List Int, Int, Int ) -> Cmd msg



-- port for listening for decoded instruction from JavaScript


port decoded : (List String -> msg) -> Sub msg


init : ( Model, Cmd AppMessage )
init =
    ( { messages = [ "Welcome to the rs-nes debugger!" ], decodedRom = [], registers = Registers.new }, Cmd.none )



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
            ( { model | messages = (("Unable to receive debugger command: " ++ msg) :: model.messages) }, Cmd.none )

        Decoded bytes ->
            ( { model | messages = ("DECODED!" :: model.messages), decodedRom = bytes }, Cmd.none )

        SetBreakpointClick address ->
            ( model, ToggleBreakpoint.request address SetBreakpointRequestFail SetBreakpointRequestSuccess )

        SetBreakpointRequestSuccess resp ->
            ( { model | messages = (("Breakpoint set at " ++ toString resp.address) :: model.messages) }, Cmd.none )

        SetBreakpointRequestFail err ->
            ( { model | messages = (("Set breakpoint fail: " ++ toString err) :: model.messages) }, Cmd.none )

        StepClick ->
            ( model, Step.request StepRequestFail StepRequestSuccess )

        StepRequestSuccess resp ->
            ( { model | messages = ("Stepped!" :: model.messages) }, Cmd.none )

        StepRequestFail err ->
            ( { model | messages = (("Step request fail: " ++ toString err) :: model.messages) }, Cmd.none )

        ContinueClick ->
            ( model, Continue.request ContinueRequestFail ContinueRequestSuccess )

        ContinueRequestSuccess resp ->
            ( { model | messages = ("Continued!" :: model.messages) }, Cmd.none )

        ContinueRequestFail err ->
            ( { model | messages = (("Continue request fail: " ++ toString err) :: model.messages) }, Cmd.none )

        CpuSnapshotRequestSuccess cpuSnapshot ->
            ( { model | messages = ("Decoding..." :: model.messages), registers = cpuSnapshot.registers }, decode ( cpuSnapshot.memory, Instructions.decodeStartRange cpuSnapshot.registers.pc, Instructions.decodeEndRange cpuSnapshot.registers.pc + 20 ) )

        CpuSnapshotRequestFail err ->
            ( { model | messages = (("Fetch Fail: " ++ toString err) :: model.messages) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ decoded (\asm -> Decoded asm)
        , WebSocket.listen wsDebuggerEndpoint <| DebuggerCommand.decode DebuggerCommandReceiveFail DebuggerCommandReceiveSuccess
        ]



-- VIEW


view : Model -> Html AppMessage
view model =
    div []
        [ button [ onClick StepClick ] [ text "Step" ]
        , button [ onClick ContinueClick ] [ text "Continue" ]
        , div [ id TwoColumn ]
            [ div [ id LeftColumn ]
                [ div []
                    [ div [] [ Registers.view model.registers ]
                    , Instructions.view model.registers.pc model.decodedRom
                    ]
                ]
            , ul [ id Messages, class [ CssCommon.List ] ] (List.map (\msg -> li [] [ text msg ]) (List.reverse model.messages))
            ]
        ]


type CssIds
    = TwoColumn
    | LeftColumn
    | Messages


styles : List Css.Snippet
styles =
    [ (#) TwoColumn
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.children
            [ Css.Elements.div
                [ Css.padding (Css.px 5)
                ]
            ]
        ]
    , (#) LeftColumn
        [ Css.borderRight3 (Css.px 1) Css.solid (Css.hex "#CCCCCC")
        ]
    , (#) Messages
        [ Css.fontFamily Css.monospace
        ]
    ]
