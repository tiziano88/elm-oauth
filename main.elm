import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task

import OAuth exposing (googleConfig, facebookConfig)


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
  }


google : OAuth.Config
google =
  { googleConfig
  | clientId = "253270339440-tp9fiqj5boaqvrs3j8g2u0mtdn4ittgp.apps.googleusercontent.com"
  , scopes = [ "https://www.googleapis.com/auth/drive" ]
  }


facebook : OAuth.Config
facebook =
  { facebookConfig
  | clientId = "299210180425495"
  , scopes = [ "public_profile" ]
  }


init : (Model, Cmd Msg)
init =
  { googleAuthClient = (OAuth.newClient google)
  , facebookAuthClient = (OAuth.newClient facebook)
  } ! []


-- UPDATE


type Msg
  = Nop
  | FacebookAuth OAuth.Msg
  | GoogleAuth OAuth.Msg
  | Drive
  | DriveResp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      model ! []

    DriveResp ->
      model ! []

    GoogleAuth m ->
      let
        (client, cmd) = OAuth.update m model.googleAuthClient
      in
        { model | googleAuthClient = client } ! [ Cmd.map GoogleAuth cmd ]

    FacebookAuth m ->
      let
        (client, cmd) = OAuth.update m model.facebookAuthClient
      in
        { model | facebookAuthClient = client } ! [ Cmd.map FacebookAuth cmd ]

    Drive ->
      model ! [ driveCmd model ]


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "OAuth Demo"]
    , button [ onClick (GoogleAuth OAuth.Auth) ] [ text "Google Auth" ]
    , button [ onClick (FacebookAuth OAuth.Auth) ] [ text "Facebook Auth" ]
    , br [] []
    , pre [] [ text (toString model) ]
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
    (send (OAuth.token model.googleAuthClient) drive [("q", "name contains 'test'")])


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
