#' @export
.Leonardo <- setClass(
    "Leonardo",
    contains = "Service",
    slots = c(api_header = "character")
)

.LEONARDO_API_REFERENCE_VERSION <- "1.3.6"

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Leonardo-class operations,Leonardo-method
#'
#' @returns `Leonardo()` creates the API of the Leonardo container
#'     deployment service at
#'     \url{https://leonardo.dsde-prod.broadinstitute.org/api-docs.yaml}.
#'     The default API url value can be changed with the
#'     `AnVIL.leonardo_api_url` option.
#'
#' @format NULL
#'
#' @importFrom GCPtools gcloud_access_token
#'
#' @examples
#' library(GCPtools)
#' if (gcloud_exists())
#'     Leonardo()
#'
#' @export
Leonardo <-
    function()
{
    access_token <- GCPtools::gcloud_access_token("leonardo")
    api_reference_url <- getOption("AnVIL.leonardo_api_url")
    api_header <- c(
        Authorization = paste("Bearer", access_token),
        Referer = .get_referer(api_reference_url)
    )
    .Leonardo(
        Service(
            "leonardo",
            host = .get_host(api_reference_url),
            authenticate = FALSE,
            api_reference_version = .LEONARDO_API_REFERENCE_VERSION,
            api_reference_url = api_reference_url
        ),
        api_header = api_header
    )
}

#' @export
setMethod(
    "operations", "Leonardo",
    function(x, ..., .deprecated = FALSE)
{
    callNextMethod(x, .headers = .api_header(x), ..., .deprecated = .deprecated)
})
