#' @export
.Rawls <- setClass(
    "Rawls",
    contains = "Service",
    slots = c(api_header = "character")
)

.RAWLS_API_REFERENCE_VERSION <- "1.0.0"

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Rawls-class operations,Rawls-method schemas,Rawls-method
#'
#' @returns `Rawls()` creates the API of the Rawls cloud computational
#'     environment at \url{https://rawls.dsde-prod.broadinstitute.org}.
#'     The default API url value can be changed with the
#'     `AnVIL.rawls_api_url` option.
#'
#' @format NULL
#'
#' @importFrom GCPtools gcloud_access_token
#'
#' @examples
#' library(GCPtools)
#' if (gcloud_exists()) {
#'     tags(Rawls())
#'     tags(Rawls(), "billing")
#' }
#'
#' @export
Rawls <-
    function()
{
    access_token <- GCPtools::gcloud_access_token("rawls")
    api_header <- c(Authorization = paste("Bearer", access_token))
    api_reference_url <- getOption("AnVIL.rawls_api_url")
    .Rawls(
        Service(
            "rawls",
            host = .get_host(api_reference_url),
            authenticate = FALSE,
            api_reference_version = .RAWLS_API_REFERENCE_VERSION,
            api_reference_url = api_reference_url
        ),
        api_header = api_header
    )
}

## Some operations seem to have a poorly-defined operationId in the json

#' @export
setMethod(
    "operations", "Rawls",
    function(x, ..., .deprecated = FALSE)
{
    callNextMethod(
        x, .headers = .api_header(x), ..., .deprecated = .deprecated
    )
})
