## sub-class to allow method dispatch

#' @export
.Dockstore <- setClass(
    "Dockstore",
    contains = "Service",
    slots = c(api_header = "character")
)

.api_header <- function(x) x@api_header
.DOCKSTORE_API_REFERENCE_VERSION <- "1.20.0"

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Dockstore-class operations,Dockstore-method
#'
#' @returns `Dockstore()` represents the API of the Dockstore platform to
#'     share Docker-based tools in CWL or WDL or Nextflow at
#'     \url{https://dockstore.org}. The default API url value can be
#'     changed with the `AnVIL.dockstore_api_url` option.
#'
#' @format NULL
#'
#' @examples
#' Dockstore()
#'
#' @export
Dockstore <-
    function()
{
    api_header <- character()
    access <- .authenticate_get_access("dockstore")
    if (!is.null(access)) {
        token <- access$token
        api_header <- c(Authorization = paste("Bearer", token))
    }
    api_reference_url <- getOption("AnVIL.dockstore_api_url")
    .Dockstore(
        Service(
            "dockstore",
            host = .get_host(api_reference_url),
            api_reference_version = .DOCKSTORE_API_REFERENCE_VERSION,
            authenticate = FALSE,
            api_reference_url = api_reference_url
        ),
        api_header = api_header
    )
}

#' @export
setMethod(
    "operations", "Dockstore",
    function(x, ..., .deprecated = FALSE)
{
    ## Use .api_header() for authentication.
    value <- callNextMethod(
        x, .headers = .api_header(x), ..., .deprecated = .deprecated
    )
    ## Some operations have a poorly defined operationId in the json
    value[grep("[_,]+", names(value), invert = TRUE)]
})
