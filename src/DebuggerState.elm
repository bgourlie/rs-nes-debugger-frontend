module DebuggerState exposing (transition, DebuggerState(..), RequestResult(..), Input(..))

import Http
import Step
import Task
import Continue


-- A state machine defining all the debugger states and transitions


type alias Model a =
    { a
        | debuggerState : DebuggerState
    }


type DebuggerState
    = NotConnected
    | Pausing
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
    | PauseRequestComplete RequestResult
    | Step
    | StepRequestComplete RequestResult
    | Continue
    | ContinueRequestComplete RequestResult


updateDebuggerState : DebuggerState -> Input -> DebuggerState
updateDebuggerState oldState input =
    case oldState of
        NotConnected ->
            case input of
                Connect ->
                    Paused

                Step ->
                    NotConnected

                _ ->
                    Unknown

        Pausing ->
            case input of
                PauseRequestComplete Success ->
                    Paused

                PauseRequestComplete Fail ->
                    Running

                _ ->
                    Unknown

        Paused ->
            case input of
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
                    Pausing

                _ ->
                    Unknown

        Unknown ->
            Unknown


transition :
    (Http.Error -> msg)
    -> (Step.Model -> msg)
    -> (Http.Error -> msg)
    -> (Continue.Model -> msg)
    -> (( DebuggerState, Input ) -> msg)
    -> Input
    -> ( Model a, Cmd msg )
    -> ( Model a, Cmd msg )
transition stepFail stepSuccess continueFail continueSuccess unknownStateHandler smInput appInput =
    let
        ( inputModel, inputCmd ) =
            appInput

        oldState =
            inputModel.debuggerState

        newState =
            updateDebuggerState oldState smInput

        newModel =
            { inputModel | debuggerState = newState }

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
