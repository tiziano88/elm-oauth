module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation
import Task
import OAuth
import OAuth.Config


main =
    Navigation.program
        (always Nop)
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { token : Maybe OAuth.Token
    }


googleAuthClient : OAuth.Client
googleAuthClient =
    OAuth.newClient
        OAuth.Config.google
        { clientId = "253270339440-tp9fiqj5boaqvrs3j8g2u0mtdn4ittgp.apps.googleusercontent.com"
        , scopes = [ "https://www.googleapis.com/auth/drive" ]
        , redirectUrl = "http://localhost:8000/main.elm"
        , authFlow = OAuth.Implicit
        }


facebookAuthClient : OAuth.Client
facebookAuthClient =
    OAuth.newClient
        OAuth.Config.facebook
        { clientId = "299210180425495"
        , scopes = [ "public_profile" ]
        , redirectUrl = "http://localhost:8000/main.elm"
        , authFlow = OAuth.Implicit
        }


digitalOceanAuthClient : OAuth.Client
digitalOceanAuthClient =
    OAuth.newClient
        OAuth.Config.digitalOcean
        { clientId = "34047f70e9d7e91befef744b49f21fc58d506e8f0c1d7681e8af7fdb4d59da55"
        , scopes = [ "read" ]
        , redirectUrl = "http://localhost:8000/main.elm"
        , authFlow = OAuth.Implicit
        }


gitHubAuthClient : OAuth.Client
gitHubAuthClient =
    OAuth.newClient
        OAuth.Config.gitHub
        { clientId = "b7941bb82bd63e684712"
        , scopes = [ "user" ]
        , redirectUrl = "http://localhost:8000/main.elm"
        , authFlow = OAuth.Implicit
        }


stackExchangeAuthClient : OAuth.Client
stackExchangeAuthClient =
    OAuth.newClient
        OAuth.Config.stackExchange
        { clientId = "7515"
        , scopes = [ "" ]
        , redirectUrl = "http://localhost:8000/main.elm"
        , authFlow = OAuth.Implicit
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { token = Nothing } ! [ OAuth.init googleAuthClient location |> Cmd.map Token ]



-- UPDATE


type Msg
    = Nop
    | Token (Result Http.Error OAuth.Token)
    | Drive
    | DriveResp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model ! []

        Token (Ok t) ->
            { model | token = Just t } ! []

        Token (Err _) ->
            model ! []

        DriveResp ->
            model ! []

        Drive ->
            model ! [ driveCmd model ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "OAuth Demo" ]
        , ul []
            [ li [] [ a [ href <| OAuth.buildAuthUrl googleAuthClient ] [ text "Google" ] ]
            , li [] [ a [ href <| OAuth.buildAuthUrl facebookAuthClient ] [ text "Facebook" ] ]
            , li [] [ a [ href <| OAuth.buildAuthUrl digitalOceanAuthClient ] [ text "DigitalOcean" ] ]
            , li [] [ a [ href <| OAuth.buildAuthUrl gitHubAuthClient ] [ text "GitHub" ] ]
            , li [] [ a [ href <| OAuth.buildAuthUrl stackExchangeAuthClient ] [ text "StackExchange" ] ]
            ]
        , pre
            [ style
                [ ( "white-space", "pre-wrap" )
                ]
            ]
            [ text (toString model) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


apiKey =
    "AIzaSyDZVUn4Hf7xI0arEW8_pmrVBO7hfjjP6sM"


api =
    "urlshortener"


version =
    "v1"


collection =
    "url"


method =
    "get"


driveCmd : Model -> Cmd Msg
driveCmd model =
    case model.token of
        Just token ->
            send token drive [ ( "q", "name contains 'test'" ) ]

        Nothing ->
            Cmd.none


newApiClient : ApiClient
newApiClient =
    { token = Nothing
    }


type alias ApiClient =
    { token : Maybe String
    }


type alias ApiDef =
    { name : String
    , version : String
    , collection : String
    , method : String
    }


type Method
    = Get (List ( String, String ))
    | Post String


urlShortener : ApiDef
urlShortener =
    { name = "urlshortener"
    , version = "v1"
    , collection = "url"
    , method = "get"
    }


drive : ApiDef
drive =
    { name = "drive"
    , version = "v3"
    , collection = "files"
    , method = "get"
    }


req : OAuth.Token -> ApiDef -> List ( String, String ) -> Http.Request String
req token def fields =
    Http.request
        { method = def.method
        , headers = getHeaders token
        , url =
            url
                ("https://content.googleapis.com/" ++ def.name ++ "/" ++ def.version ++ "/" ++ def.collection)
                (fields ++ [ ( "key", apiKey ) ])
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


getHeaders : OAuth.Token -> List Http.Header
getHeaders token =
    case token of
        OAuth.Validated t ->
            [ Http.header "Authorization" ("Bearer " ++ t) ]


send : OAuth.Token -> ApiDef -> List ( String, String ) -> Cmd Msg
send token def fields =
    Http.send handle (req token def fields)


handle r =
    case r of
        Ok v ->
            Nop

        Err _ ->
            Nop



-- Copied from https://github.com/evancz/elm-http/blob/master/src/Http.elm#L56


url : String -> List ( String, String ) -> String
url baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    Http.encodeUri key ++ "=" ++ Http.encodeUri value
