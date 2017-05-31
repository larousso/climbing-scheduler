module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type Mode
    = Login
    | CreateAccount


type alias Model =
    { mode : Mode }


type Msg
    = SetMode Mode


init : ( Model, Cmd Msg )
init =
    ( Model Login, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


liClass : Mode -> String -> Attribute msg
liClass mode liName =
    case ( mode, liName ) of
        ( Login, "login" ) ->
            classList [ ( "active", True ) ]

        ( CreateAccount, "createAccount" ) ->
            classList [ ( "active", True ) ]

        ( _, _ ) ->
            classList []


mainContent : Mode -> Html Msg
mainContent mode =
    case mode of
        Login ->
            div [ class "tab-content" ]
                [ div [ class "form-item" ]
                    [ label [] [ text "Login" ]
                    , input [ type_ "text", placeholder "login" ] []
                    ]
                , div [ class "form-item" ]
                    [ label [] [ text "password" ]
                    , input [ type_ "password", placeholder "password" ] []
                    ]
                , button [ class "btn", type_ "button" ] [ text "Se connecter" ]
                ]

        CreateAccount ->
            div [ class "tab-content" ]
                [ div [ class "form-item" ]
                    [ label [] [ text "Login" ]
                    , input [ type_ "text", placeholder "login" ] []
                    ]
                , div [ class "form-item" ]
                    [ label [] [ text "password" ]
                    , input [ type_ "password", placeholder "password" ] []
                    ]
                , button [ class "btn", type_ "button" ] [ text "Créer son compte" ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "box-container" ]
            [ div [ class "box" ] []
            , div [ class "box-6" ]
                [ div [ class "big-title-layout" ]
                    [ div [ class "box big-title-box grey red-text" ] [ text "C" ]
                    , div [ class "box big-title-box grey red-text" ] [ text "L" ]
                    , div [ class "box big-title-box grey yellow-text" ] [ text "I" ]
                    , div [ class "box big-title-box grey red-text" ] [ text "M" ]
                    , div [ class "box big-title-box grey red-text" ] [ text "B" ]
                    , div [ class "box big-title-box grey yellow-text" ] [ text "I" ]
                    , div [ class "box big-title-box grey red-text" ] [ text "N" ]
                    , div [ class "box big-title-box grey red-text" ] [ text "G" ]
                    ]
                ]
            , div [ class "box" ] []
            ]
        , div [ class "box-container" ]
            [ div [ class "box" ]
                [ div [ class "subtitle-layout" ]
                    [ div [ class "subtitle-box" ] [ text "Scheduler" ]
                    ]
                ]
            ]
        , div [ class "main-container" ]
            [ div [ class "tab" ]
                [ ul []
                    [ li [ liClass model.mode "login" ] [ a [ href "#", onClick (SetMode Login) ] [ text "Se connecter" ] ]
                    , li [ liClass model.mode "createAccount" ] [ a [ href "#", onClick (SetMode CreateAccount) ] [ text "Créer un compte" ] ]
                    ]
                , mainContent model.mode
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
