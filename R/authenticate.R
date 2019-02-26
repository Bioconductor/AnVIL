authenticate_path <- function(service)
    system.file(package="AnVIL", "service", service, "auth.json")

authenticate_ok <-
    function(service)
{
    path <- authenticate_path(service)
    test <- file.exists(path)
    if (!test)
        warning(
            "'", service, "' requires additional configuration; ",
            "see `?authenticate`",
            call. = FALSE
        )
    invisible(test)
}

#' @rdname authenticate
#'
#' @title Authenticate against the google cloud app `anvil-leo-dev`
#'
#' @param service character(1) name of AnVIL service (e.g.,
#'     "leonardo", "terra") requiring authentication.
#'
#' @param cache (optional) logical(1) or character(1) describing how
#'     the OAuth 2.0 token will be managed. See
#'     `?httr:oauth2.0_token`.
#'
#' @return An 'R6' token instance containing the authorization,
#'     invisibly.
#'
#' @details AnVIL applications require OAuth 2.0 credentials
#'     identifying the application. These must be added to the package
#'     source before installation.
#'
#'     For Terra and Leonardo, visit
#'     `https://console.cloud.google.com/apis/credentials?authuser=1&project=anvil-leo-dev`
#'     and download (click on the downward-facing arrow to the
#'     right) the "Bioconductor-AnVIL" credentials to a file
#'     `inst/service/{leonardo,terra}/auth.json`.
#'
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @importFrom jsonlite read_json
#' @export
authenticate <-
    function(service, cache = getOption("httr_oauth_cache"))
{
    stopifnot(.is_scalar_character(service))

    path <- authenticate_path(service)
    (interactive() && file.exists(path)) || return(invisible(NULL))

    access <- read_json(path)
    ## FIXME: auth.json in leonardo has 'installed' element, terra
    ## does not. Probably this is a mis-understanding about how
    ## authentication works
    if ("installed" %in% names(access))
        access <- access$installed

    app <- oauth_app(
        "Leonardo",
        key = access$client_id,
        secret = access$client_secret
    )

    token <- oauth2.0_token(
        oauth_endpoints("google"), app,
        scope = "openid email",
        cache = cache
    )

    invisible(token)
}

authenticate_config <-
    function(service)
{
    token <- authenticate(service)
    httr::config(token = token)
}
