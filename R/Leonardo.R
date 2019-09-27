#' @export
.Leonardo <- setClass(
    "Leonardo",
    contains = "Service",
    slots = c(api_header = "character")
)

## construct a singleton instance for this service

#' @rdname Services
#'
#' @aliases Leonardo-class operations,Leonardo-method
#'
#' @return `Leonardo()` creates the API of the Leonard container
#'     deployment service at https://leonardo.dev.anvilproject.org/.
#'
#' @format NULL
#'
#' @export
Leonardo <-
    function()
{
    access_token <- .gcloud_access_token()
    api_header <- c(Authorization = paste("Bearer", access_token))
    .Leonardo(
        Service(
            "leonardo",
            host = "notebooks.firecloud.org",
            config = httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L),
            api_url = "https://notebooks.firecloud.org/api-docs.yaml",
            authenticate = FALSE
        ),
        api_header = api_header
    )
}

#' @export
setMethod(
    "operations", "Leonardo",
    function(x)
{
    callNextMethod(x, .headers = .api_header(x))
})
