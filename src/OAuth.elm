module OAuth
    exposing
        ( Client
        , ServerConfig
        , ClientConfig
        , Token(..)
        , AuthFlow(..)
        , init
        , buildAuthUrl
        , newClient
        )

{-| This library allows handling OAuth 2.0 authentication.

# Client

@docs Client, newClient

# Configuration

@docs ServerConfig, ClientConfig

# Token

@docs Token

# App

@docs buildAuthUrl, init

-}

import Dict
import Http
import Json.Decode
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
    { authorizeUrl : String
    , tokenUrl : String
    , validateUrl : String
    }


{-| Client-side configuration for a single OAuth client.
It includes the endpoints used to obtain and verify tokens, and also client-specific settings.
-}
type alias ClientConfig =
    { clientId : String
    , scopes : List String
    , redirectUrl : String
    , authFlow : AuthFlow
    }


{-| They type of authorization flow (or grant) to use.

- AuthorizationCode: https://tools.ietf.org/html/rfc6749#section-4.1
- Implicit: https://tools.ietf.org/html/rfc6749#section-4.2
-}
type AuthFlow
    = AuthorizationCode
    | Implicit


{-| An OAuth client.
-}
type alias Client =
    { serverConfig : ServerConfig
    , clientConfig : ClientConfig
    }


{-| See https://tools.ietf.org/html/rfc6749#section-4.1.4 .
-}
type alias AccessTokenResponse =
    { accessToken : String
    , tokenType : String
    , expiresIn : Int
    }


accessTokenResponseDecoder : Json.Decode.Decoder AccessTokenResponse
accessTokenResponseDecoder =
    Json.Decode.map3 AccessTokenResponse
        (Json.Decode.field "access_token" Json.Decode.string)
        (Json.Decode.field "token_type" Json.Decode.string)
        (Json.Decode.field "expires_in" Json.Decode.int)


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


{-| A function to create an initial Cmd to be used with a `Navigation.program` init function.

    type Msg
        = Token (Result Http.Error OAuth.Token)
        ...

    init : Navigation.Location -> ( Model, Cmd Msg )
    init location =
        { ... } ! [ OAuth.init client location |> Cmd.map Token ]
-}
init : Client -> Navigation.Location -> Cmd (Result Http.Error Token)
init client =
    case client.clientConfig.authFlow of
        Implicit ->
            getTokenFromHash >> validateToken client

        AuthorizationCode ->
            getCodeFromQuery >> exchangeCode client



-- TODO: Generate and verify nonce.


{-| Builds an URL that when followed allows the user to authenticate with the specified provider.
-}
buildAuthUrl : Client -> String
buildAuthUrl client =
    url
        client.serverConfig.authorizeUrl
        [ ( "response_type", "token" )
        , ( "immediate", "true" )
        , ( "approval_prompt", "auto" )
        , ( "client_id", client.clientConfig.clientId )
        , ( "redirect_uri", client.clientConfig.redirectUrl )
        , ( "scope", String.join " " client.clientConfig.scopes )
        ]


{-| Builds an URL to request an access token.
See https://tools.ietf.org/html/rfc6749#section-4.1.3 .
-}
buildTokenUrl : Client -> String -> String
buildTokenUrl client code =
    url
        client.serverConfig.tokenUrl
        [ ( "grant_type", "authorization_code" )
        , ( "code", code )
        , ( "client_id", client.clientConfig.clientId )
        , ( "redirect_uri", client.clientConfig.redirectUrl )
        ]


buildValidateUrl : Client -> String -> String
buildValidateUrl client token =
    url
        client.serverConfig.validateUrl
        [ ( "input_token", token )
        , ( "access_token", token )
        ]


getTokenFromHash : Navigation.Location -> String
getTokenFromHash loc =
    let
        params =
            parseUrlParams loc.hash
    in
        Dict.get "access_token" params
            |> Maybe.withDefault ""


getCodeFromQuery : Navigation.Location -> String
getCodeFromQuery loc =
    let
        params =
            parseUrlParams loc.search
    in
        Dict.get "code" params
            |> Maybe.withDefault ""


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
    s
        |> String.dropLeft 1
        |> String.split "&"
        |> List.map parseSingleParam
        |> Dict.fromList


parseSingleParam : String -> ( String, String )
parseSingleParam p =
    let
        s =
            String.split "=" p
    in
        case s of
            [ s1, s2 ] ->
                ( s1, s2 )

            _ ->
                ( "", "" )


validateToken : Client -> String -> Cmd (Result Http.Error Token)
validateToken client token =
    Http.getString (buildValidateUrl client token)
        |> Http.send
            (\r ->
                case r of
                    Ok _ ->
                        Ok (Validated token)

                    Err e ->
                        Err e
            )


exchangeCode : Client -> String -> Cmd (Result Http.Error Token)
exchangeCode client code =
    Http.post (buildTokenUrl client code) Http.emptyBody accessTokenResponseDecoder
        |> Http.send
            (\r ->
                case r of
                    Ok v ->
                        Ok (Validated v.accessToken)

                    Err e ->
                        Err e
            )



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
