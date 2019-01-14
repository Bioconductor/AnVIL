## sub-class to allow method dispatch

#' @export
.Dockstore <- setClass("Dockstore", contains = "Service")

## construct a singleton instance for this service

#' @rdname Service
#'
#' @aliases Dockstore-class operations, Dockstore-method schemas
#'
#' @return `dockstore` represents the API of the Dockstore platform to
#'     share Docker-based tools in CWL or WDL or Nextflow at
#'     https://dockstore.org/api/
#'
#' @export
dockstore <- .Dockstore(Service("dockstore", host="dockstore.org/api/"))

## Some operations seem to have a poorly defined operationId in the json

#' @export
setMethod(
    "operations", "Dockstore",
    function(x)
{
    value <- callNextMethod()
    value[grep("[_,]+", names(value), invert = TRUE)]
})
