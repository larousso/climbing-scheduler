port module Main exposing (..)

import Html exposing (..)


--import Html.App as App

import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http as Http exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field, decodeString)
import Debug
import Native.Navigation
import Date exposing (..)


type Msg
    = Dispose


type alias Flags =
    { date : String }


type alias Model =
    { date : Date }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case fromString flags.date of
        Ok d ->
            ( Model d, Cmd.none )

        Err error ->
            Debug.crash "Wrong date format"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Dispose ->
            ( model, onDispose "leave" )


port onDispose : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style [ ( "position", "absolute" ), ( "top", "-50px" ), ( "left", "0" ) ] ]
        [ text <| toString model.date
        , button [ onClick Dispose ] [ text "dispose" ]
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
