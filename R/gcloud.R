.gcloud_do <-
    function(...)
{
    .gcloud_sdk_do("gcloud", c(...))
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
