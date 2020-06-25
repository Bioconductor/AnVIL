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

#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#'
#' @importFrom jsonlite read_json
authenticate <-
    function(service, cache = getOption("httr_oauth_cache"))
{
    interactive() || return(invisible(NULL))
    stopifnot(.is_scalar_character(service))

    access <- list(
        client_id = getOption("anvil_client_id"),
        client_secret = getOption("anvil_client_secret")
    )

    path <- authenticate_path(service)
    if (file.exists(path)) {
        access <- read_json(path)
        if ("installed" %in% names(access))
            access <- access$installed
    }

    app <- oauth_app(
        "AnVILBiocPackage",
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
