setOldClass("response")

#' @rdname Response
#'
#' @name Response
#'
#' @aliases flatten,response-method
#'
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
NULL

#' @export
setMethod("flatten", "response",
    function(x)
{
    json <- fromJSON(content(x, as="text", encoding = "UTF-8"), flatten = TRUE)
    as_tibble(json)
})

#' @rdname Response
#'
#' @param object A `response` object returned by the service.
#'
#' @return `str()` displays a compact representation of the list-like
#'     JSON response; it returns `NULL`.
#'
#' @examples
#' \donttest{leonardo$getSystemStatus() %>% str()}
#'
#' @export
setMethod("str", "response",
    function(object)
{
    json <- fromJSON(content(object, as="text", encoding = "UTF-8"))
    str(json)
})

#' @rdname Response
#' @param as a character(1); one of 'raw', 'text', 'parsed'
#' @param \dots not currently used
#' @return a list with the content of the response
#' @export
as.list.response <-
    function(x, ..., as="text")
{
    jsonlite::fromJSON(httr::content(x, as=as))
}

