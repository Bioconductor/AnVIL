## sub-class to allow method dispatch
#' @export
.Terra <- setClass(
    "Terra",
    contains = "Service",
    slots = c(api_header = "character")
)

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Terra-class operations,Terra-method schemas,Terra-method
#'
#' @return `Terra()` creates the API of the Terra cloud computational
#'     environemnt at https://api.firecloud.org/.
#' 
#' @format NULL
#'
#' @export
Terra <-
    function()
{
    bearer_token <- .gcloud_do("auth", "print-access-token")
    api_header <- c(Authorization = paste("Bearer", bearer_token))
    .Terra(
        Service(
            "terra",
            host = "api.firecloud.org",
            api_url = "https://api.firecloud.org/api-docs.yaml",
            authenticate = FALSE
        ),
        api_header = api_header
    )
}

## Some operations seem to have a poorly-defined operationId in the json

#' @export
setMethod(
    "operations", "Terra",
    function(x)
{
    value <- callNextMethod(x, .headers = .api_header(x))
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
