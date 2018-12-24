#' @rdname authenticate
#'
#' @title Authenticate against the google cloud app `anvil-leo-dev`
#'
#' @param cache (optional) logical(1) or character(1) describing how
#'     the OAuth 2.0 token will be managed. See
#'     `?httr:oauth2.0_token`.
#'
#' @return An 'R6' token instance containing the authorization,
#'     invisibly.
#'
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
authenticate <-
    function(cache = getOption("httr_oauth_cache"))
{
    app <- oauth_app(
        "Leonardo",
        key = anvil("leonardo_access")$client_id,
        secret = anvil("leonardo_access")$client_secret
    )

    token <- oauth2.0_token(
        oauth_endpoints("google"), app,
        scope = "openid email",
        cache = cache
    )

    invisible(token)
}
