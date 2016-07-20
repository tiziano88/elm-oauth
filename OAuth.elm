module OAuth exposing
  ( Client
  , ServerConfig
  , ClientConfig
  , Msg (Auth)
  , Token (..)
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
Normally embedded in the application model.

    type alias Model =
      { ...
      , authClient : OAuth.Client
      , ...
      }

-}
type alias Client =
  { serverConfig : ServerConfig
  , clientConfig : ClientConfig
  , token : Maybe Token
  }


{-| Creates a new OAuth client based on a configuration.
Normally used when initialising the application model.

    init : (Model, Cmd Msg)
    init = {
      { ...
      , authClient = (OAuth.newClient serverConfig clientConfig)
      , ...
      }

-}
newClient : ServerConfig -> ClientConfig -> Client
newClient serverConfig clientConfig =
  { serverConfig = serverConfig
  , clientConfig = clientConfig
  , token = Nothing
  }


{-| Extracts a validated token from an existing OAuth client, if present.
-}
getToken : Client -> Maybe Token
getToken = .token


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
          (initAuthFlow (buildAuthUrl client))
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
