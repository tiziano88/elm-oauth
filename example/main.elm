import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation
import Task

import OAuth
import OAuth.Config


main =
  Navigation.program (OAuth.urlParser digitalOceanAuthClient)
    { init = init
    , view = view
    , update = update
    , urlUpdate = urlUpdate
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
    }


facebookAuthClient : OAuth.Client
facebookAuthClient =
  OAuth.newClient
    OAuth.Config.facebook
    { clientId = "299210180425495"
    , scopes = [ "public_profile" ]
    , redirectUrl = "http://localhost:8000/main.elm"
    }


digitalOceanAuthClient : OAuth.Client
digitalOceanAuthClient =
  OAuth.newClient
    OAuth.Config.digitalOcean
    { clientId = "34047f70e9d7e91befef744b49f21fc58d506e8f0c1d7681e8af7fdb4d59da55"
    , scopes = [ "read" ]
    , redirectUrl = "http://localhost:8000/main.elm"
    }


gitHubAuthClient : OAuth.Client
gitHubAuthClient =
  OAuth.newClient
    OAuth.Config.gitHub
    { clientId = "b7941bb82bd63e684712"
    , scopes = [ "user" ]
    , redirectUrl = "http://localhost:8000/main.elm"
    }


stackExchangeAuthClient : OAuth.Client
stackExchangeAuthClient =
  OAuth.newClient
    OAuth.Config.stackExchange
    { clientId = "7515"
    , scopes = [ "" ]
    , redirectUrl = "http://localhost:8000/main.elm"
    }


init : (Task.Task String OAuth.Token) -> (Model, Cmd Msg)
init task =
  { token = Nothing
  } ! [ Task.perform (always Nop) Token task ]


-- UPDATE


type Msg
  = Nop
  | Token OAuth.Token
  | Drive
  | DriveResp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      model ! []

    Token t ->
      { model
      | token = Just t
      } ! []

    DriveResp ->
      model ! []

    Drive ->
      model ! [ driveCmd model ]


urlUpdate : (Task.Task String OAuth.Token) -> Model -> (Model, Cmd Msg)
urlUpdate task model =
  (model, Task.perform (always Nop) (always Nop) task)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "OAuth Demo"]
    , ul []
      [ li [] [ a [ href <| OAuth.buildAuthUrl googleAuthClient ] [ text "Google" ] ]
      , li [] [ a [ href <| OAuth.buildAuthUrl facebookAuthClient ] [ text "Facebook" ] ]
      , li [] [ a [ href <| OAuth.buildAuthUrl digitalOceanAuthClient ] [ text "DigitalOcean" ] ]
      , li [] [ a [ href <| OAuth.buildAuthUrl gitHubAuthClient ] [ text "GitHub" ] ]
      , li [] [ a [ href <| OAuth.buildAuthUrl stackExchangeAuthClient ] [ text "StackExchange" ] ]
      ]
    , pre
      [ style
        [ ("white-space", "pre-wrap")
        ]
      ]
      [ text (toString model) ]
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP

apiKey = "AIzaSyDZVUn4Hf7xI0arEW8_pmrVBO7hfjjP6sM"


api = "urlshortener"
version = "v1"
collection = "url"
method = "get"


driveCmd : Model -> Cmd Msg
driveCmd model =
  case model.token of
    Just token ->
      Task.perform
        (always DriveResp)
        (always DriveResp)
        (send token drive [("q", "name contains 'test'")])

    Nothing -> Cmd.none


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
  = Get (List (String, String))
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


req : OAuth.Token -> ApiDef -> List (String, String) -> Http.Request
req token def fields =
  { verb = def.method
  , headers = getHeaders token
  , url =
    Http.url
      ("https://content.googleapis.com/" ++ def.name ++ "/" ++ def.version ++ "/" ++ def.collection)
      (fields ++ [("key", apiKey)])
  , body = Http.empty
  }


getHeaders : OAuth.Token -> List (String, String)
getHeaders token =
  case token of
    OAuth.Validated t -> [("Authorization", "Bearer " ++ t)]


send : OAuth.Token -> ApiDef -> List (String, String) -> Task.Task Http.Error String
send token def fields =
  Http.send Http.defaultSettings (req token def fields)
    |> Task.mapError promoteError
    |> Task.map (\resp -> stringValue resp.value)


promoteError : Http.RawError -> Http.Error
promoteError rawError =
  case rawError of
    Http.RawTimeout -> Http.Timeout
    Http.RawNetworkError -> Http.NetworkError


stringValue : Http.Value -> String
stringValue v =
  case v of
    Http.Text str -> str
    _ -> ""
