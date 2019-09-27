.gcloud_do <-
    function(...)
{
    .gcloud_sdk_do("gcloud", c(...))
}        

.gcloud_access_token <-
    function()
{
    app_default <-
        if (identical(Sys.getenv("USER"), "jupyter-user"))
            "application-default"
    .gcloud_do("auth", app_default, "print-access-token")
}

#' @export
gcloud_account <- function()
    .gcloud_do("config", "get-value", "account")

#' @export
gcloud_project <- function()
    .gcloud_do("config", "get-value", "project")

#' @export
gcloud_help <- function(...)
    noquote(.gcloud_do("help", ...))

#' @export
gcloud_cmd <- function(cmd, ...)
    .gcloud_do(cmd, ...)
