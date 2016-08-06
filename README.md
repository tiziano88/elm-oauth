# elm-oauth

An OAuth client for elm.

Notes:

- Only Implicit OAuth 2.0 web-based flow (no client secret) is supported.
- Refreshing tokens is not supported (this is only available in the
  Authorization Code flow, which requires the cooperation of a server and cannot
  be implemented purely client-side).
- Tokens normally expire after one hour (actual length depends on the provider
  used).
- Authenticating requires navigating to a URL on a different domain, and that is
  only possible by letting the user click on a plain HTML link
  (`<a href="...">`); this means that the browser will navigate to a different
  page and the current state of the application will be lost.
- Passing a state (or nonce) to the authentication request in order to prevent
  request forgery attacks is not supported (this is because it is not possible
  to keep such generated state in memory and verify it after the authentication
  callback, since the application is restarted in the meanwhile and all its
  state is lost).

See also:

- https://tools.ietf.org/html/rfc6749
- https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2
- http://oauthbible.com/#oauth-2-two-legged
