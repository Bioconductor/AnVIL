.gcloud_do <-
    function(...)
{
    .gcloud_sdk_do("gcloud", c(...))
}        

gcloud_account <- function()
    .gcloud_do("config", "get-value", "account")

gcloud_project <- function()
    .gcloud_do("config", "get-value", "project")
