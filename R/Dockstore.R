## sub-class to allow method dispatch

#' @export
.Dockstore <- setClass(
    "Dockstore",
    contains = "Service",
    slots = c(api_header = "character")
)

.api_header <- function(x) x@api_header

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Dockstore-class operations,Dockstore-method
#'
#' @return `Dockstore()` represents the API of the Dockstore platform to
#'     share Docker-based tools in CWL or WDL or Nextflow at
#'     https://dockstore.org
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
    path <- authenticate_path("dockstore")
    if (file.exists(path)) {
        token <- read_json(path)$token
        api_header <- c(Authorization = paste("Bearer", token))
    }
    .Dockstore(
        Service(
            "dockstore", host="dockstore.org", authenticate = FALSE,
            api_url = "https://dockstore.org/swagger.json",
            api_reference_md5sum = "d70ecce0e117b00bbf483af09423dc54"
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
