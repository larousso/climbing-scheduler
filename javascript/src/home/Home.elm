module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http as Http exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field, decodeString)
import Debug
import Native.Navigation


type Mode
    = Login
    | CreateAccount


type alias Account =
    { login : String, password : String }


type alias Model =
    { mode : Mode, account : Account, error : Maybe String }


type alias User =
    { id : String, login : String }


type alias Session =
    { userLogin : String, role : String }


type Msg
    = SetMode Mode
    | SetLogin String
    | SetPassword String
    | SendCreateAccount
    | SendLogin
    | CreateAccountResponse (Result Http.Error User)
    | LoginResponse (Result Http.Error Session)


init : ( Model, Cmd Msg )
init =
    ( Model Login (Account "" "") Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SetLogin login ->
            let
                oldAccount =
                    model.account

                newAccount =
                    { oldAccount | login = login }
            in
                ( { model | account = newAccount, error = Nothing }, Cmd.none )

        SetPassword password ->
            let
                oldAccount =
                    model.account

                newAccount =
                    { oldAccount | password = password }
            in
                ( { model | account = newAccount, error = Nothing }, Cmd.none )

        SendCreateAccount ->
            ( model, createAccount model.account )

        SendLogin ->
            ( model, doLogin model.account )

        CreateAccountResponse (Ok _) ->
            ( model, doLogin model.account )

        CreateAccountResponse (Err (BadStatus response)) ->
            let
                errMess =
                    decodeString errorDecoder response.body
            in
                case errMess of
                    Ok msg ->
                        ( { model | error = Just msg }, Cmd.none )

                    Err _ ->
                        ( { model | error = Just "Une erreur bizarre est survenue" }, Cmd.none )

        CreateAccountResponse (Err _) ->
            ( { model | error = Just "Une erreur bizarre est survenue" }, Cmd.none )

        LoginResponse (Ok session) ->
            ( model, Native.Navigation.setLocation <| "/home/@" ++ session.userLogin )

        LoginResponse (Err (BadStatus response)) ->
            ( { model | error = Just "Une erreur bizarre est survenue" }, Cmd.none )

        LoginResponse (Err _) ->
            ( { model | error = Just "Une erreur bizarre est survenue" }, Cmd.none )


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


doLogin : Account -> Cmd Msg
doLogin account =
    let
        url =
            "/api/login"

        request =
            Http.post url (jsonAccount account |> Http.jsonBody) sessionDecoder
    in
        Http.send LoginResponse request


createAccount : Account -> Cmd Msg
createAccount account =
    let
        url =
            "/api/users"

        request =
            Http.post url (jsonAccount account |> Http.jsonBody) userDecoder
    in
        Http.send CreateAccountResponse request


jsonAccount : Account -> Encode.Value
jsonAccount account =
    let
        list =
            [ ( "login", Encode.string account.login )
            , ( "password", Encode.string account.password )
            ]
    in
        list
            |> Encode.object


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (field "id" Decode.string)
        (field "login" Decode.string)


sessionDecoder : Decode.Decoder Session
sessionDecoder =
    Decode.map2 Session
        (field "userLogin" Decode.string)
        (field "role" Decode.string)


errorDecoder : Decode.Decoder String
errorDecoder =
    (field "error" Decode.string)


mainContent : Model -> Html Msg
mainContent model =
    case model.mode of
        Login ->
            div [ class "tab-content" ]
                [ div [ class "form-item" ]
                    [ label [] [ text "Login" ]
                    , input [ type_ "text", placeholder "login", value model.account.login, onInput SetLogin ] []
                    ]
                , div [ class "form-item" ]
                    [ label [] [ text "password" ]
                    , input [ type_ "password", placeholder "password", value model.account.password, onInput SetPassword ] []
                    ]
                , button [ class "btn", type_ "button", onClick SendLogin ] [ text "Se connecter" ]
                ]

        CreateAccount ->
            div [ class "tab-content" ]
                [ div [ class "errorMessage" ] [ text (Maybe.withDefault "" model.error) ]
                , div [ class "form-item" ]
                    [ label [] [ text "Login" ]
                    , input [ type_ "text", placeholder "login", value model.account.login, onInput SetLogin ] []
                    ]
                , div [ class "form-item" ]
                    [ label [] [ text "password" ]
                    , input [ type_ "password", placeholder "password", value model.account.password, onInput SetPassword ] []
                    ]
                , button [ class "btn", type_ "button", onClick SendCreateAccount ] [ text "Créer son compte" ]
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
                , mainContent model
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
