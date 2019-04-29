## sub-class to allow method dispatch

#' @export
.Terra <- setClass("Terra", contains = "Service")

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Terra-class operations,Terra-method schemas,Terra-method
#'
#' @return `terra` represents the API of the Terra cloud computational
#'     environemnt at https://api.firecloud.org/.
#' 
#' @format NULL
#'
#' @export
terra <- NULL

Terra <-
    function()
{
    .Terra(Service("terra", host = "api.firecloud.org"))
}

## Some operations seem to have a poorly-defined operationId in the json

#' @export
setMethod(
    "operations", "Terra",
    function(x)
{
    value <- callNextMethod()
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
