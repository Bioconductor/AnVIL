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
#' @return `TDR()` creates the API of the Terra Data Repository to work with
#'   snapshot data in the Terra Data Repository at \url{https://data.terra.bio}.
#'
#' @format NULL
#'
#' @examples
#' if (gcloud_exists())
#'     TDR()
#'
#' @export
TDR <-
    function()
{
    access_token <- .gcloud_access_token("tdr")
    api_header <- c(
        Authorization = paste("Bearer", access_token)
    )
    .TDR(
        Service(
            "tdr",
            host = "data.terra.bio",
            config = httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L),
            api_reference_version = .TDR_API_REFERENCE_VERSION,
            authenticate = FALSE,
            api_reference_url =
                "https://data.terra.bio/data-repository-openapi.yaml"
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
