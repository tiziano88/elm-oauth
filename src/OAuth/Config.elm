module OAuth.Config exposing (..)

{-| This library contains several ready-made server configs for various OAuth 2.0 providers

@docs google, facebook, gitHub, stackExchange

-}

import OAuth

{-| Base configuration for Google endpoints
Based on https://developers.google.com/identity/protocols/OAuth2UserAgent .
-}
google : OAuth.ServerConfig
google =
  { endpointUrl = "https://accounts.google.com/o/oauth2/v2/auth"
  , validateUrl = "https://www.googleapis.com/oauth2/v3/tokeninfo"
  }


{-| Base configuration for Facebook endpoints
Based on https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow .
-}
facebook : OAuth.ServerConfig
facebook =
  { endpointUrl = "https://www.facebook.com/dialog/oauth"
  , validateUrl = "https://graph.facebook.com/debug_token"
  }


{-| Base configuration for GitHub endpoints
Based on https://developer.github.com/v3/oauth/ .
Note: Does not currently work. It seems that GitHub does not currently support web-only flow.
-}
gitHub : OAuth.ServerConfig
gitHub =
  { endpointUrl = "https://github.com/login/oauth/authorize"
  , validateUrl = ""
  }


{-| Base configuration for StackExchange endpoints
Based on https://api.stackexchange.com/docs/authentication .
Note: Verification does not seem to be provided by this endpoint.
-}
stackExchange : OAuth.ServerConfig
stackExchange =
  { endpointUrl = "https://stackexchange.com/oauth/dialog"
  , validateUrl = ""
  }
