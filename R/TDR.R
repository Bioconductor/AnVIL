#' @exportClass TDR
.TDR <- setClass(
    "TDR",
    contains = "Service",
    slots = c(api_header = "character")
)

.api_header <- function(x) x@api_header
.TDR_API_REFERENCE_VERSION <- "0.1.0"

#' @rdname Services
#'
#' @aliases TDR-class operations,TDR-method
#'
#' @returns `TDR()` creates the API of the Terra Data Repository to work with
#'   snapshot data in the Terra Data Repository at \url{https://data.terra.bio}.
#'   The default API url value can be changed with the
#'   `AnVIL.tdr_api_url` option.
#'
#' @format NULL
#'
#' @importFrom GCPtools gcloud_access_token
#'
#' @examples
#' library(GCPtools)
#' if (gcloud_exists())
#'     TDR()
#'
#' @export
TDR <-
    function()
{
    access_token <- GCPtools::gcloud_access_token("tdr")
    api_header <- c(
        Authorization = paste("Bearer", access_token)
    )
    api_reference_url <- getOption("AnVIL.tdr_api_url")
    .TDR(
        Service(
            "tdr",
            host = .get_host(api_reference_url),
            api_reference_version = .TDR_API_REFERENCE_VERSION,
            authenticate = FALSE,
            api_reference_url = api_reference_url
        ),
        api_header = api_header
    )
}


#' @export
setMethod(
    "operations", "TDR",
    function(x, ..., .deprecated = FALSE)
{
    callNextMethod(
        x, .headers = .api_header(x), ..., .deprecated = .deprecated
    )
})
