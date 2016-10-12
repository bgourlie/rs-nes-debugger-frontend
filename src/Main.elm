module Main exposing (..)

import Html exposing (Html, Attribute, div, text, input, button, label, p)
import Html.App as App
import Html.Attributes as Attrs exposing (style, type', value, min, max)
import Color exposing (Color, toRgb)
import Json.Decode as Json


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
    }


init : ( Model, Cmd AppMessage )
init =
    ( { message = "Hello, world!" }, Cmd.none )



-- UPDATE


type AppMessage
    = NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html AppMessage
view model =
    div [] [ text model.message ]
