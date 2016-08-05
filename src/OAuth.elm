module OAuth exposing
  ( Client
  , ServerConfig
  , ClientConfig
  , Token (..)
  , buildAuthUrl
  , newClient
  , urlParser
  )

{-| This library allows handling OAuth 2.0 authentication.

# Client

@docs Client, newClient

# Configuration

@docs ServerConfig, ClientConfig

# Token

@docs Token

# App

@docs buildAuthUrl, urlParser

-}

import Dict
import Http
import Navigation
import String
import Task


{-| Represents a validated OAuth token.
-}
type Token
  = Validated String


{-| Server-side configuration for a single OAuth client.
It includes the endpoints used to obtain and verify tokens, and also client-specific settings.
-}
type alias ServerConfig =
  { endpointUrl : String
  , validateUrl : String
  }


{-| Client-side configuration for a single OAuth client.
It includes the endpoints used to obtain and verify tokens, and also client-specific settings.
-}
type alias ClientConfig =
  { clientId : String
  , scopes : List String
  , redirectUrl : String
  }


{-| An OAuth client.
-}
type alias Client =
  { serverConfig : ServerConfig
  , clientConfig : ClientConfig
  }


{-| Creates a new OAuth client based on a server configuration and a client configuration.
Normally defined at top-level in the application.

    authClient : OAuth.Client
    authClient = OAuth.newClient serverConfig clientConfig

-}
newClient : ServerConfig -> ClientConfig -> Client
newClient serverConfig clientConfig =
  { serverConfig = serverConfig
  , clientConfig = clientConfig
  }


{-| Builds an URL that when followed allows the user to authenticate with the specified provider.
-}
-- TODO: Generate and verify nonce.
buildAuthUrl : Client -> String
buildAuthUrl client =
  Http.url
    client.serverConfig.endpointUrl
    [ ("response_type", "code token")
    , ("immediate", "true")
    , ("approval_prompt", "auto")
    , ("client_id", client.clientConfig.clientId)
    , ("redirect_uri", client.clientConfig.redirectUrl)
    , ("scope", String.join " " client.clientConfig.scopes)
    ]


buildValidateUrl : Client -> String -> String
buildValidateUrl client token =
  Http.url
    client.serverConfig.validateUrl
    [ ("input_token", token)
    , ("access_token", token)
    ]


{-| A function to create a URL parser to be used with a `Navigation.program`.

    main =
      Navigation.program (OAuth.urlParser authClient)
        { init = ...
        , view = ...
        , update = ...
        , urlUpdate = ...
        , subscriptions = ...
        }

-}
urlParser : Client -> Navigation.Parser (Task.Task String Token)
urlParser client =
  Navigation.makeParser (.hash >> getTokenFromHash >> validateToken client)


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
