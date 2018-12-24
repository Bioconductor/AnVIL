#' @title Get or set AnVIL options
#'
#' @description View available options and current values with
#'     `anvil()`.  Retrieve specific option with, e.g.,
#'     `anvil("leonardo_host")`.  Set anvil with
#'     `anvil("leonardo_host", <value>)`.
#'
#' @details AnVIL requires OAuth 2.0 credentials identifying the
#'     application. These must be added to the package source before
#'     installation. Do this by visiting
#'     `https://console.cloud.google.com/apis/credentials?authuser=1&project=anvil-leo-dev` and
#'     downloading the "Bioconductor-AnVIL" credentials to a file
#'     `inst/extdata/leonardo_access.json`.
#'
#' @param key character(1) name of option.
#'
#' @param value ANY value associated with key.
#'
#' @return `AnVIL::anvil()` returns a named list of key-value
#'     pairs.
#'
#'     `AnVIL::anvil("leonardo_host")` returns the value of option
#'     `"leonardo_host"`.
#'
#'     `AnVIL::anvil("key", "value")` updates the option `key` to
#'     `value`, returning the previous value invisibly.
#'
#' @examples
#' anvil("leonardo_host")
#' @importFrom jsonlite read_json
#' @export
anvil <- local({

    options <- new.env(parent=emptyenv())

    options[["leonardo_host"]] <- "https://leonardo.dev.anvilproject.org"
    options[["leonardo_config"]] <-
        httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
    delayedAssign(
        "leonardo_access", .anvil_leonardo_access(), assign.env = options
    )
    options[["leonardo_scope"]] <-
        list(
            profile = "https://www.googleapis.com/auth/userinfo.profile",
            email = "https://www.googleapis.com/auth/userinfo.email",
            openid = "https://www.googleapis.com/auth/plus.me"
        )
        
    function(key, value) {
        if (missing(key) && missing(value)) {
            as.list(options)
        } else if (missing(value)) {
            .anvil_check_key(key, options)
            options[[key]]
        } else if (missing(key)) {
            stop("'key' required when 'value' provided")
        } else {
            .anvil_check_key(key, options)
            previous <- options[[key]]
            options[[key]] <- value
            previous
        }
    }
})

.anvil_leonardo_access <-
    function()
{
    leonardo_access <- system.file(
        package="AnVIL", "extdata", "leonardo_access.json"
    )
    if (!nzchar(leonardo_access))
        stop("AnVIL installation incomplete; see ?anvil")
    read_json(leonardo_access)$installed
}

.anvil_check_key <- function(key, options)
    stopifnot(.is_scalar_character(key), key %in% names(options))
