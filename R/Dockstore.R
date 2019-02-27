## sub-class to allow method dispatch

#' @export
.Dockstore <- setClass(
    "Dockstore",
    contains = "Service",
    slots = c(api_header = "character")
)

.api_header <- function(x) x@api_header

## construct a singleton instance for this service

#' @rdname Service
#'
#' @aliases Dockstore-class operations,Dockstore-method
#'
#' @return `dockstore` represents the API of the Dockstore platform to
#'     share Docker-based tools in CWL or WDL or Nextflow at
#'     https://dockstore.org
#'
#' @export
dockstore <- NULL

Dockstore <-
    function()
{
    api_header <- NULL
    path <- authenticate_path("dockstore")
    if (file.exists(path)) {
        token <- read_json(path)$token
        api_header <- c(Authorization = paste("Bearer", token))
    }
    .Dockstore(
        Service("dockstore", host="dockstore.org", authenticate_config = FALSE),
        api_header = api_header
    )
}

#' @export
setMethod(
    "operations", "Dockstore",
    function(x)
{
    ## Use .api_header() for authentication.
    value <- get_operations(.api(x), .headers = .api_header(x))
    ## Some operations have a poorly defined operationId in the json
    value[grep("[_,]+", names(value), invert = TRUE)]
})
