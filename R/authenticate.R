#' @importFrom keyring key_get key_set_with_value key_list
.authenticate_get_access <-
    function(service)
{
    access <- NULL

    # Check keyring (service = "AnVIL", username = <service>)
    tryCatch({
        kl <- key_list("AnVIL")
        if (service %in% kl$username) {
            content <- key_get("AnVIL", service)
            access <- jsonlite::fromJSON(content)
        }
    }, error = function(e) {
        # ignore keyring errors, fallback to old path
    })

    # Fallback to insecure path for backward compatibility, with warning
    if (is.null(access)) {
        path <- system.file(package="AnVIL", "service", service, "auth.json")
        if (nzchar(path) && file.exists(path)) {
             warning(
                "Reading 'auth.json' from package directory is insecure and",
                " deprecated.\nUse 'anvil_set_auth_json()' to move credentials",
                " to a secure keyring.",
                call. = FALSE
            )
            access <- jsonlite::read_json(path)
        }
    }

    access
}

#' @title Store and retrieve authentication credentials using a secure keyring
#'
#' @description `anvil_set_auth_json()` stores the content of an `auth.json`
#'   file in the system keyring. This is the recommended way to store
#'   credentials safely.
#'
#' @param service `character(1)` The name of the service (e.g., `"terra"`,
#'     `"dockstore"`) for which the credentials are being set.
#'
#' @param path `character(1)` The path to the `auth.json` file.
#'
#' @return `anvil_set_auth_json()` returns `NULL` invisibly.
#'
#' @importFrom keyring key_set_with_value
#'
#' @examplesIf interactive()
#' jsonlite::write_json(
#'    list(token = "example_token"),
#'    "terratcgadata-test-key.json",
#' )
#' anvil_set_auth_json(
#'     "terra",
#'     "terratcgadata-test-key.json"
#' )
#' AnVIL:::.authenticate_get_access("terra")
#' unlink("terratcgadata-test-key.json")
#'
#' @export
anvil_set_auth_json <-
    function(service, path)
{
    stopifnot(
        isScalarCharacter(service),
        isScalarCharacter(path),
        file.exists(path)
    )

    jsonlite::read_json(path)

    content <- readChar(path, file.info(path)$size)
    key_set_with_value("AnVIL", service, password = content)
}

authenticate_ok <-
    function(service)
{
    access <- .authenticate_get_access(service)
    test <- !is.null(access)
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
    stopifnot(isScalarCharacter(service))

    access <- .authenticate_get_access(service)

    if (is.null(access)) {
        access <- list(
            client_id = getOption("anvil_client_id"),
            client_secret = getOption("anvil_client_secret")
        )
    } else if ("installed" %in% names(access)) {
        access <- access$installed
    }

    app <- oauth_app(
        appname = "AnVILBiocPackage",
        key = access$client_id,
        secret = access$client_secret
    )

    token <- oauth2.0_token(
        endpoint = oauth_endpoints("google"),
        app = app,
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
