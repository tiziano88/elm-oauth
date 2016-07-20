import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task

import OAuth
import OAuth.Config


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { googleAuthClient : OAuth.Client
  , facebookAuthClient : OAuth.Client
  , gitHubAuthClient : OAuth.Client
  , stackExchangeAuthClient : OAuth.Client
  }


google : OAuth.ClientConfig
google =
  { clientId = "253270339440-tp9fiqj5boaqvrs3j8g2u0mtdn4ittgp.apps.googleusercontent.com"
  , scopes = [ "https://www.googleapis.com/auth/drive" ]
  , redirectUrl = "http://localhost:8000/main.elm"
  }


facebook : OAuth.ClientConfig
facebook =
  { clientId = "299210180425495"
  , scopes = [ "public_profile" ]
  , redirectUrl = "http://localhost:8000/main.elm"
  }


gitHub : OAuth.ClientConfig
gitHub =
  { clientId = "b7941bb82bd63e684712"
  , scopes = [ "user" ]
  , redirectUrl = "http://localhost:8000/main.elm"
  }


stackExchange : OAuth.ClientConfig
stackExchange =
  { clientId = "7515"
  , scopes = [ "" ]
  , redirectUrl = "http://localhost:8000/main.elm"
  }


init : (Model, Cmd Msg)
init =
  { googleAuthClient = (OAuth.newClient OAuth.Config.google google)
  , facebookAuthClient = (OAuth.newClient OAuth.Config.facebook facebook)
  , gitHubAuthClient = (OAuth.newClient OAuth.Config.gitHub gitHub)
  , stackExchangeAuthClient = (OAuth.newClient OAuth.Config.stackExchange stackExchange)
  } ! []


-- UPDATE


type Msg
  = Nop
  | FacebookAuth OAuth.Msg
  | GoogleAuth OAuth.Msg
  | GitHubAuth OAuth.Msg
  | StackExchangeAuth OAuth.Msg
  | Drive
  | DriveResp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      model ! []

    DriveResp ->
      model ! []

    GoogleAuth authMsg ->
      let
        (client, cmd) = OAuth.update authMsg model.googleAuthClient
      in
        { model | googleAuthClient = client } ! [ Cmd.map GoogleAuth cmd ]

    FacebookAuth authMsg ->
      let
        (client, cmd) = OAuth.update authMsg model.facebookAuthClient
      in
        { model | facebookAuthClient = client } ! [ Cmd.map FacebookAuth cmd ]

    GitHubAuth authMsg ->
      let
        (client, cmd) = OAuth.update authMsg model.gitHubAuthClient
      in
        { model | gitHubAuthClient = client } ! [ Cmd.map GitHubAuth cmd ]

    StackExchangeAuth authMsg ->
      let
        (client, cmd) = OAuth.update authMsg model.stackExchangeAuthClient
      in
        { model | stackExchangeAuthClient = client } ! [ Cmd.map StackExchangeAuth cmd ]

    Drive ->
      model ! [ driveCmd model ]


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "OAuth Demo"]
    , button [ onClick (GoogleAuth OAuth.Auth) ] [ text "Google Auth" ]
    , button [ onClick (FacebookAuth OAuth.Auth) ] [ text "Facebook Auth" ]
    , button [ onClick (GitHubAuth OAuth.Auth) ] [ text "GitHub Auth" ]
    , button [ onClick (StackExchangeAuth OAuth.Auth) ] [ text "StackExchange Auth" ]
    , br [] []
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
  Task.perform
    (always DriveResp)
    (always DriveResp)
    (send (OAuth.getToken model.googleAuthClient) drive [("q", "name contains 'test'")])


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


req : Maybe OAuth.Token -> ApiDef -> List (String, String) -> Http.Request
req token def fields =
  { verb = def.method
  , headers = getHeaders token
  , url =
    Http.url
      ("https://content.googleapis.com/" ++ def.name ++ "/" ++ def.version ++ "/" ++ def.collection)
      (fields ++ [("key", apiKey)])
  , body = Http.empty
  }


getHeaders : Maybe OAuth.Token -> List (String, String)
getHeaders token =
  case token of
    Just (OAuth.Validated t) -> [("Authorization", "Bearer " ++ t)]
    _ -> []


send : Maybe OAuth.Token -> ApiDef -> List (String, String) -> Task.Task Http.Error String
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
