port module Main exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li)
import Html.App as App
import List exposing (map)


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
    }



-- port for sending decode requests out to JavaScript


port decode : List Int -> Cmd msg



-- port for listening for decoded instruction from JavaScript


port decoded : (List String -> msg) -> Sub msg


init : ( Model, Cmd AppMessage )
init =
    ( { message = "Hello, world!", decodedRom = [] }, decode testRom )


testRom : List Int
testRom =
    [ 0x61
    , 0x00
    , 0x65
    , 0x00
    , 0x69
    , 0x00
    , 0x6D
    , 0x0F
    , 0xF0
    , 0x71
    , 0x00
    , 0x75
    , 0x00
    , 0x79
    , 0x00
    , 0x00
    , 0x7D
    , 0x00
    , 0x00
    ]



-- UPDATE


type AppMessage
    = Decoded (List String)
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        Decoded bytes ->
            ( { model | message = "DECODED!", decodedRom = bytes }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ decoded (\asm -> Decoded asm)
        ]



-- VIEW


view : Model -> Html AppMessage
view model =
    div []
        [ div [] [ text model.message ]
        , div [] [ instructionList model ]
        ]


instructionList : Model -> Html AppMessage
instructionList model =
    ul [] (map (\str -> li [] [ text str ]) model.decodedRom)
