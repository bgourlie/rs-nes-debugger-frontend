module AppState exposing (AppState(..), Input(..), transition)

import Continue
import Step
import Task


-- A state machine defining all the debugger states and transitions


type alias Model a =
    { a
        | appState : AppState
    }


type AppState
    = NotConnected
    | Paused
    | Running


type Input
    = Connect
    | Disconnect
    | Pause
    | Continue
    | Step


transition : Input -> AppState -> Result ( Input, AppState ) AppState
transition input oldState =
    case oldState of
        NotConnected ->
            case input of
                Connect ->
                    Ok Paused

                _ ->
                    Err ( input, oldState )

        Paused ->
            case input of
                Pause ->
                    Ok Paused

                Disconnect ->
                    Ok NotConnected

                Continue ->
                    Ok Running

                Step ->
                    Ok Running

                _ ->
                    Err ( input, oldState )

        Running ->
            case input of
                Disconnect ->
                    Ok NotConnected

                Pause ->
                    Ok Paused

                Continue ->
                    Ok Running

                Step ->
                    Ok Running

                _ ->
                    Err ( input, oldState )
