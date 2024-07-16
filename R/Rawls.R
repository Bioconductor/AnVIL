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
#' @return `Rawls()` creates the API of the Rawls cloud computational
#'     environemnt at \url{https://rawls.dsde-prod.broadinstitute.org}.
#'
#' @format NULL
#'
#' @examples
#' library(AnVILGCP)
#' if (gcloud_exists()) {
#'     tags(Rawls())
#'     tags(Rawls(), "billing")
#' }
#'
#' @export
Rawls <-
    function()
{
    if (!requireNamespace("AnVILGCP", quietly = TRUE))
        stop("Install 'AnVILGCP' to use 'Rawls()'", call. = FALSE)
    access_token <- AnVILGCP::gcloud_access_token("rawls")
    api_header <- c(Authorization = paste("Bearer", access_token))
    .Rawls(
        Service(
            "rawls",
            host = "rawls.dsde-prod.broadinstitute.org",
            authenticate = FALSE,
            api_reference_version = .RAWLS_API_REFERENCE_VERSION,
            api_reference_url =
                "https://rawls.dsde-prod.broadinstitute.org/api-docs.yaml"
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
