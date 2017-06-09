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
    = DayClicked String Int Int


type alias Flags =
    { user : String }


type alias CalendarEvent =
    { date : Date, posX : Int, posY : Int }


type alias Model =
    { mode : String, user : String, dayClicked : Maybe CalendarEvent }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" flags.user Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayClicked date x y ->
            case fromString date of
                Ok d ->
                    let
                        calendarEvent =
                            CalendarEvent d x y
                    in
                        Debug.log "New date"
                            ( { model | dayClicked = Just calendarEvent }, Cmd.none )

                Err error ->
                    Debug.crash ("Error parsing date : " ++ date)
                        ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    dayClicked (\( pos, x, y ) -> DayClicked pos x y)


port events : String -> Cmd msg


port dayClicked : (( String, Int, Int ) -> msg) -> Sub msg


displayNewEvent : Maybe CalendarEvent -> List (Html.Html msg)
displayNewEvent mayBeEvent =
    case mayBeEvent of
        Just event ->
            []

        Nothing ->
            []


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
                , div [] (displayNewEvent model.dayClicked)
                ]
            ]
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
