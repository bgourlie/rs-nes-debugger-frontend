module AppState exposing (transition, AppState(..), RequestResult(..), Input(..))

import Http
import Step
import Task
import Continue


-- A state machine defining all the debugger states and transitions


type alias Model a =
    { a
        | appState : AppState
    }


type AppState
    = NotConnected
    | Paused
    | Stepping
    | Continuing
    | Running
    | Unknown


type RequestResult
    = Success
    | Fail


type Input
    = Connect
    | Disconnect
    | Pause
    | Step
    | StepRequestComplete RequestResult
    | Continue
    | ContinueRequestComplete RequestResult


updateAppState : AppState -> Input -> AppState
updateAppState oldState input =
    case oldState of
        NotConnected ->
            case input of
                Connect ->
                    Paused

                Step ->
                    NotConnected

                _ ->
                    Unknown

        Paused ->
            case input of
                Pause ->
                    Paused

                Disconnect ->
                    NotConnected

                Continue ->
                    Continuing

                Step ->
                    Stepping

                _ ->
                    Unknown

        Stepping ->
            case input of
                Step ->
                    Stepping

                StepRequestComplete _ ->
                    Paused

                _ ->
                    Unknown

        Continuing ->
            case input of
                Continue ->
                    Continuing

                ContinueRequestComplete Success ->
                    Running

                ContinueRequestComplete Fail ->
                    Paused

                _ ->
                    Unknown

        Running ->
            case input of
                Disconnect ->
                    NotConnected

                Pause ->
                    Paused

                Continue ->
                    Running

                _ ->
                    Unknown

        Unknown ->
            Unknown


transition :
    (Http.Error -> msg)
    -> (Step.Model -> msg)
    -> (Http.Error -> msg)
    -> (Continue.Model -> msg)
    -> (( AppState, Input ) -> msg)
    -> Input
    -> ( Model a, Cmd msg )
    -> ( Model a, Cmd msg )
transition stepFail stepSuccess continueFail continueSuccess unknownStateHandler smInput appInput =
    let
        ( inputModel, inputCmd ) =
            appInput

        oldState =
            inputModel.appState

        newState =
            updateAppState oldState smInput

        newModel =
            { inputModel | appState = newState }

        newCmd =
            if newState == oldState then
                inputCmd
            else
                case newState of
                    Stepping ->
                        Cmd.batch [ inputCmd, Step.request stepFail stepSuccess ]

                    Continuing ->
                        Cmd.batch [ inputCmd, Continue.request continueFail continueSuccess ]

                    Unknown ->
                        Cmd.batch [ inputCmd, Task.perform unknownStateHandler (Task.succeed ( oldState, smInput )) ]

                    _ ->
                        inputCmd
    in
        ( newModel, newCmd )
