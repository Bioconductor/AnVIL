#' @export
.Terra <- setClass(
    "Terra",
    contains = "Service",
    slots = c(api_header = "character")
)

.TERRA_API_REFERENCE_VERSION <- "0.1"

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Terra-class operations,Terra-method schemas,Terra-method
#'
#' @returns `Terra()` creates the API of the Terra cloud computational
#'     environment at \url{https://api.firecloud.org/}. The default
#'     API url value can be changed with the `AnVIL.firecloud_api_url`
#'     option.
#'
#' @format NULL
#'
#' @importFrom GCPtools gcloud_access_token
#'
#' @examples
#' library(GCPtools)
#' if (gcloud_exists()) {
#'     tags(Terra())
#'     tags(Terra(), "Billing")
#' }
#'
#' @export
Terra <-
    function()
{
    access_token <- GCPtools::gcloud_access_token("terra")
    api_header <- c(Authorization = paste("Bearer", access_token))
    api_reference_url <- getOption("AnVIL.firecloud_api_url")
    .Terra(
        Service(
            "terra",
            host = .get_host(api_reference_url),
            authenticate = FALSE,
            api_reference_version = .TERRA_API_REFERENCE_VERSION,
            api_reference_url = api_reference_url
        ),
        api_header = api_header
    )
}

## Some operations seem to have a poorly-defined operationId in the json

#' @export
setMethod(
    "operations", "Terra",
    function(x, ..., .deprecated = FALSE)
{
    value <- callNextMethod(
        x, .headers = .api_header(x), ..., .deprecated = .deprecated
    )
    value[grep("[_,]+", names(value), invert = TRUE)]
})

#' @export
setMethod(
    "schemas", "Terra",
    function(x)
{
    value <- callNextMethod()
    value[grep("[_,]+", names(value), invert = TRUE)]
})
