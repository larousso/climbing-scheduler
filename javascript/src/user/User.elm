port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http as Http exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field, decodeString)
import Debug
import Native.Navigation


type Msg
    = Toto


type alias Model =
    { mode : String }


init : ( Model, Cmd Msg )
init =
    ( Model "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toto ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ nav [ classList [ ( "navbar", True ), ( "navbar-default", True ) ] ]
            [ div [ class "box-container" ]
                [ div [ classList [ ( "navbar-header", True ) ] ]
                    [ a [ class "brand" ]
                        [ span [ class "red-text" ] [ text "Climbing" ]
                        , span [ class "yellow-text" ] [ text "scheduler" ]
                        ]
                    ]
                ]
            ]
        , div [ classList [ ( "main-container", True ) ] ]
            [ div [ classList [ ( "light-zone", True ), ( "main-zone", True ) ] ]
                [ h1 [ class "center" ] [ text "My scheduler" ]
                , div [ id "calendar" ] []
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
