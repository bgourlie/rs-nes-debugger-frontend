port module Ports exposing (..)

{-| Given a class name, scroll the first element with that class into view.
-}


port scrollElementIntoViewCommand : String -> Cmd msg
