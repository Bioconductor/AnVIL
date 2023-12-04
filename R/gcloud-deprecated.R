##
## gcloud_sdk_result constructor and methods
##
.gcloud_sdk_result <-
    function(x)
{
    if (!is.null(x))
        class(x) <- "gcloud_sdk_result"
    x
}

#' @export
print.gcloud_sdk_result <-
    function(x, ...)
{
    if (is.null(x))
        return()
    cat(noquote(x), sep="\n")
}


## option or environment variable or NULL, allowing for default
## `unset` value
.gcloud_sdk_getenv <-
    function(option, unset = NA)
{
    value <- getOption(option, unset)
    if (!is.na(value))
        return(value)

    value <- Sys.getenv(option, unset = unset)
    if (!is.na(value))
        return(value)

    return(NULL)
}

## Get gsutil binary on user's machine for windows/linux/mac
.gcloud_sdk_find_binary <-
    function(binary_name)
{
    user_path <- .gcloud_sdk_getenv("GCLOUD_SDK_PATH")
    if (!is.null(user_path))
        return(normalizePath(file.path(user_path, "bin", binary_name)))

    bin <- Sys.which(binary_name)
    if (nzchar(bin))
        return(normalizePath(bin))

    ## Discover binary automatically if user doesn't give path
    if (.Platform$OS.type == "windows") {
        appdata <- normalizePath(Sys.getenv("localappdata"), winslash = "/")
        sdk_path <- file.path("Google", "Cloud SDK", "google-cloud-sdk", "bin")
        binary_cmd <- paste(binary_name, "cmd", sep = ".")
        bin_paths <- c(
            file.path(appdata, sdk_path, binary_cmd),
            file.path(Sys.getenv("ProgramFiles"), sdk_path, binary_cmd),
            file.path(Sys.getenv("ProgramFiles(x86)"), sdk_path, binary_cmd)
        )
    } else {
        bin_paths <- file.path("~", "google-cloud-sdk", "bin", binary_name)
    }

    ## Return appropriate path for 'gsutil'
    for (path in bin_paths)
        if (file.exists(path))
            return(normalizePath(path))

    stop(
        "failed to find '", binary_name, "' binary; ",
        "set option or environment variable 'GCLOUD_SDK_PATH'?",
        call. = FALSE
    )
}

.gcloud_sdk_do <-
    function(command, args)
{
    stopifnot(
        .is_scalar_character(command),
        .is_character(args, na.ok = FALSE)
    )
    bin <- .gcloud_sdk_find_binary(command)
    stopifnot(file.exists(bin))

    value <- withCallingHandlers({
        tryCatch({
            system2(bin, args, stdout = TRUE, stderr = TRUE, wait=TRUE)
        }, error = function(err) {
            msg <- paste0(
                "'", command, " ", paste(args, collapse = " "), "' failed:\n",
                "  ", conditionMessage(err)
            )
            stop(msg, call. = FALSE)
        })
    }, warning = function(warn) {
        invokeRestart("muffleWarning")
    })

    if (!is.null(attr(value, "status"))) {
        msg <- paste0(
            "'", command, " ", paste(args, collapse = " "), "' failed:",
            "\n  ", paste(as.vector(value), collapse = "\n    "),
            "\n  exit status: ", attr(value, "status")
        )
        stop(msg, call. = FALSE)
    }

    value
}

.gcloud_do <-
    function(...)
{
    .gcloud_sdk_do("gcloud", c(...))
}

.gcloud_access_token_new <-
    function(app_default, now)
{
    ## obtain the access token
    token <- .gcloud_do("auth", app_default, "print-access-token")

    ## There is only one token per service account, so requesting a
    ## token may return one with expiration less than 60 minutes. So
    ## check the actual expiry time of the token.
    ##
    ## Calculating expiry from before the call (`now` argument) is
    ## conservative -- we underestimate the time by the latency
    ## involved in the POST and result parsing.
    response <- POST(
        "https://www.googleapis.com/oauth2/v1/tokeninfo",
        httr::content_type("application/x-www-form-urlencoded"),
        body = paste0("access_token=", token)
    )
    .avstop_for_status(response, ".gcloud_access_token_expires")
    expires <- now + content(response)$expires_in

    list(token = token, expires = expires)
}

.gcloud_access_token <-
    local(
{
    tokens <- new.env(parent = emptyenv())
    function(service) {
        app_default <-
            if (identical(Sys.getenv("USER"), "jupyter-user"))
                "application-default"

        key <- paste0(service, ":", app_default, ":", gcloud_account())
        now <- Sys.time()
        if (is.null(tokens[[key]])) {
            tokens[[key]] <- .gcloud_access_token_new(app_default, now)
        } else {
            expires_in <- tokens[[key]]$expires - now
            if (expires_in < 1L) {
                ## allow a nearly expired token to fully expire
                if (expires_in > 0L)
                    Sys.sleep(expires_in)
                tokens[[key]] <- .gcloud_access_token_new(app_default, now)
            }
        }

        tokens[[key]]$token
    }
})

#' @rdname gcloud-deprecated
#'
#' @name gcloud-deprecated
#'
#' @aliases gcloud
#'
#' @title gcloud and gsutil command line utility interfaces (DEPRECATED)
#'
#' @description These functions invoke the `gcloud` and `gsutil` command line
#'     utilities. See \link{gsutil} for details on how `gcloud` is
#'     located.
NULL

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_exists()` tests whether the `gcloud()` command
#'     can be found on this system. See 'Details' section of `gsutil`
#'     for where the application is searched.
#'
#' @return `gcloud_exists()` returns `TRUE` when the `gcloud`
#'     application can be found, FALSE otherwise.
#'
#' @examples
#' gcloud_exists()
#'
#' @export
gcloud_exists <-
    function()
{
    .life_cycle(
        newpackage = "AnVILGCP", cycle = "deprecated", title = "gcloud"
    )
    result <- tryCatch({
        .gcloud_sdk_find_binary("gcloud")
    }, error = function(...) "")
    nchar(result) > 0L
}

#' @importFrom utils tail
.gcloud_get_value_check <-
    function(result, function_name)
{
    value <- tail(result, 1L)
    if (identical(value, "(unset)")) {
        message <- paste0(
            "'", function_name, "()' returned '(unset)'; this may indicate ",
            "that the gcloud active configuration is incorrect. Try ",
            "`gcloud auth application-default login` at the command line"
        )
        warning(paste(strwrap(message), collapse = "\n"))
    }
    value
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_account()`: report the current gcloud account
#'     via `gcloud config get-value account`.
#'
#' @param account character(1) Google account (e.g., `user@gmail.com`)
#'     to use for authentication.
#'
#' @return `gcloud_account()` returns a `character(1)` vector
#'     containing the active gcloud account, typically a gmail email
#'     address.
#'
#' @examples
#' if (gcloud_exists())
#'     gcloud_account()
#'
#' @export
gcloud_account <- function(account = NULL) {
    .life_cycle(
        newpackage = "AnVILGCP", cycle = "deprecated", title = "gcloud"
    )
    stopifnot(is.null(account) || .is_scalar_character(account))

    if (!is.null(account))
        .gcloud_do("config", "set", "account", account)
    result <- .gcloud_do("config", "get-value", "account")
    .gcloud_get_value_check(result, "gcloud_account")
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_project()`: report the current gcloud project
#'     via `gcloud config get-value project`.
#'
#' @param project character(1) billing project name.
#'
#' @return `gcloud_project()` returns a `character(1)` vector
#'     containing the active gcloud project.
#'
#' @export
gcloud_project <- function(project = NULL) {
    stopifnot(
        is.null(project) || .is_scalar_character(project)
    )
    .life_cycle(
        newpackage = "AnVILGCP", cycle = "deprecated", title = "gcloud"
    )
    if (!is.null(project))
        .gcloud_do("config", "set", "project", project)
    result <- .gcloud_do("config", "get-value", "project")
    ## returns two lines when `CLOUDSDK_ACTIVE_CONFIG_NAME=`
    ## envirionment variable is set
    .gcloud_get_value_check(result, "gcloud_account")
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_help()`: queries `gcloud` for help for a
#'     command or sub-comand via `gcloud help ...`.
#'
#' @param ... Additional arguments appended to gcloud commands.
#'
#' @return `gcloud_help()` returns an unquoted `character()` vector
#'     representing the text of the help manual page returned by
#'     `gcloud help ...`.
#'
#' @examples
#' if (gcloud_exists())
#'     gcloud_help()
#'
#' @export
gcloud_help <- function(...) {
    .life_cycle(
        newpackage = "AnVILGCP", cycle = "deprecated", title = "gcloud"
    )
    .gcloud_sdk_result(.gcloud_do("help", ...))
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_cmd()` allows arbitrary `gcloud` command
#'     execution via `gcloud ...`. Use pre-defined functions in
#'     preference to this.
#'
#' @param cmd `character(1)` representing a command used to evaluate
#'     `gcloud cmd ...`.
#'
#' @return `gcloud_cmd()` returns a `character()` vector representing
#'     the text of the output of `gcloud cmd ...`
#'
#' @export
gcloud_cmd <- function(cmd, ...) {
    .life_cycle(
        newpackage = "AnVILGCP", cycle = "deprecated", title = "gcloud"
    )
    .gcloud_do(cmd, ...)
}
