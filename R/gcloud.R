.gcloud_do <-
    function(...)
{
    .gcloud_sdk_do("gcloud", c(...))
}

.gcloud_access_token <-
    local(
{
    tokens <- new.env(parent = emptyenv())
    function(service) {
        app_default <-
            if (identical(Sys.getenv("USER"), "jupyter-user"))
                "application-default"

        key <- paste0(service, ":", app_default)
        now <- Sys.time()

        if (is.null(tokens[[key]]) || tokens[[key]]$expires < now) {
            token <- .gcloud_do("auth", app_default, "print-access-token")
            expires <- Sys.time() + 3600L
            tokens[[key]] <- list(token = token, expires = expires)
        }
        tokens[[key]]$token
    }
})

#' @rdname gcloud
#'
#' @name gcloud
#'
#' @title gcloud command line utility interface
#'
#' @description These functions invoke the `gcloud` command line
#'     utility. See \link{gsutil} for details on how `gcloud` is
#'     located.
NULL

#' @rdname gcloud
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
    result <- tryCatch({
        .gcloud_sdk_find_binary("gcloud")
    }, error = function(...) "")
    nchar(result) > 0L
}

#' @rdname gcloud
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
    stopifnot(is.null(account) || .is_scalar_character(account))

    if (!is.null(account))
        .gcloud_do("config", "set", "account", account)
    .gcloud_do("config", "get-value", "account")
}

#' @rdname gcloud
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

    if (!is.null(project))
        .gcloud_do("config", "set", "project", project)
    .gcloud_do("config", "get-value", "project")
}

#' @rdname gcloud
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
gcloud_help <- function(...)
    .gcloud_sdk_result(.gcloud_do("help", ...))

#' @rdname gcloud
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
gcloud_cmd <- function(cmd, ...)
    .gcloud_do(cmd, ...)
