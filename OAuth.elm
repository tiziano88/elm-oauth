module OAuth exposing
  ( Client
  , Config
  , Msg (Auth)
  , Token (..)
  , googleConfig
  , facebookConfig
  , gitHubConfig
  , stackExchangeConfig
  , newClient
  , update
  , getToken
  )

import Dict
import Http
import String
import Task

import Native.OAuth


type Token
  = Validated String


type Msg
  = Nop
  | Auth
  | VerifyAuth (Result String Token)
  | Hash String


{-| Configuration for a single OAuth client.
It includes the endpoints used to obtain and verify tokens, and also client-specific settings.
-}
type alias Config =
  { endpointUrl : String
  , validateUrl : String
  , clientId : String
  , scopes : List String
  , redirectUrl : String
  }


{-| Base configuration for Google endpoints
Based on https://developers.google.com/identity/protocols/OAuth2UserAgent .
-}
googleConfig : Config
googleConfig =
  { endpointUrl = "https://accounts.google.com/o/oauth2/v2/auth"
  , validateUrl = "https://www.googleapis.com/oauth2/v3/tokeninfo"
  , clientId = ""
  , scopes = []
  , redirectUrl = ""
  }


{-| Base configuration for Facebook endpoints
Based on https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow .
-}
facebookConfig : Config
facebookConfig =
  { endpointUrl = "https://www.facebook.com/dialog/oauth"
  , validateUrl = "https://graph.facebook.com/debug_token"
  , clientId = ""
  , scopes = []
  , redirectUrl = ""
  }


{-| Base configuration for GitHub endpoints
Based on https://developer.github.com/v3/oauth/ .
Note: Does not currently work. It seems that GitHub does not currently support web-only flow.
-}
gitHubConfig : Config
gitHubConfig =
  { endpointUrl = "https://github.com/login/oauth/authorize"
  , validateUrl = ""
  , clientId = ""
  , scopes = []
  , redirectUrl = ""
  }


{-| Base configuration for StackExchange endpoints
Based on https://api.stackexchange.com/docs/authentication .
Note: Verification does not seem to be provided by this endpoint.
-}
stackExchangeConfig : Config
stackExchangeConfig =
  { endpointUrl = "https://stackexchange.com/oauth/dialog"
  , validateUrl = ""
  , clientId = ""
  , scopes = []
  , redirectUrl = ""
  }


{-| An OAuth client.
Normally embedded in the application model.

    type alias Model =
      { ...
      , authClient : OAuth.Client
      , ...
      }

-}
type alias Client =
  { config : Config
  , token : Maybe Token
  }


{-| Creates a new OAuth client based on a configuration.
Normally used when initialising the application model.

    init : (Model, Cmd Msg)
    init = {
      { ...
      , authClient = (OAuth.newClient config)
      , ...
      }

-}
newClient : Config -> Client
newClient config =
  { config = config
  , token = Nothing
  }


{-| Extracts a validated token from an existing OAuth client, if present.
-}
getToken : Client -> Maybe Token
getToken = .token


-- TODO: Generate and verify nonce.
buildAuthUrl : Config -> String
buildAuthUrl config =
  Http.url
    config.endpointUrl
    [ ("response_type", "code token")
    , ("immediate", "true")
    , ("approval_prompt", "auto")
    , ("client_id", config.clientId)
    , ("redirect_uri", config.redirectUrl)
    , ("scope", String.join " " config.scopes)
    ]


buildValidateUrl : Client -> String -> String
buildValidateUrl client token =
  Http.url
    client.config.validateUrl
    [ ("input_token", token)
    , ("access_token", token)
    ]


getTokenFromHash : String -> String
getTokenFromHash s =
  let
    params = parseUrlParams s
  in
    Dict.get "access_token" params
      |> Maybe.withDefault ""


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
  s
    |> String.dropLeft 1
    |> String.split "&"
    |> List.map parseSingleParam
    |> Dict.fromList


parseSingleParam : String -> (String, String)
parseSingleParam p =
  let
    s = String.split "=" p
  in
    case s of
      [s1,s2] -> (s1,s2)
      _ -> ("", "")


validateToken : Client -> String -> Task.Task String Token
validateToken client token =
  Http.getString (buildValidateUrl client token)
    |> Task.mapError (always "error")
    |> Task.map (always (Validated token))


update : Msg -> Client -> (Client, Cmd Msg)
update msg client =
  case msg of
    Nop ->
      client ! []

    Auth ->
      client !
        [ Task.perform
          (always Nop)
          Hash
          (initAuthFlow (buildAuthUrl client.config))
        ]

    VerifyAuth r ->
      case r of
        Ok t ->
          { client | token = Just t } ! []
        _ ->
          client ! []

    Hash hash ->
      let
        token = getTokenFromHash hash
      in
        client !
          [ Task.perform
            VerifyAuth
            VerifyAuth
            (Task.toResult (validateToken client token))
          ]


-- NATIVE


initAuthFlow : String -> Task.Task Never String
initAuthFlow =
  Native.OAuth.initAuthFlow
