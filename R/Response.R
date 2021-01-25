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

#' @rdname Response
#'
#' @name Response
#'
#' @title Process service responses to tibble and other data structures.
#'
#' @aliases flatten
#'
#' @param x A `response` object returned by the service.
#'
#' @return `flatten()` returns a `tibble` where each row correseponds
#'     to a top-level list element of the return value, and columns
#'     are the unlisted second and more-nested elements.
#'
#' @examples
#' if (gcloud_exists()) {
#'     leonardo <- Leonardo()
#'     leonardo$listClusters() %>% flatten()
#' }
#'
#' @export
setGeneric("flatten", function(x) standardGeneric("flatten"))

#' @export
setMethod("flatten", "response",
    function(x)
{
    value <- content(x, as="text", encoding = "UTF-8")
    if (nzchar(value)) {
        json <- fromJSON(value, flatten = TRUE)
        as_tibble(json)
    } else {
        tibble()
    }
})

#' @rdname Response
#'
#' @param object A `response` object returned by the service.
#'
#' @return `str()` displays a compact representation of the list-like
#'     JSON response; it returns `NULL`.
#'
#' @examples
#' if (gcloud_exists())
#'    leonardo$getSystemStatus() %>% str()
#'
#' @export
setMethod("str", "response",
    function(object)
{
    value <- content(object, as="text", encoding = "UTF-8")
    if (nzchar(value)) {
        json <- fromJSON(value)
    } else {
        json <- character()
    }
    str(json)
})

#' @rdname Response
#'
#' @param as character(1); one of 'raw', 'text', 'parsed'
#'
#' @param \dots not currently used
#'
#' @return `as.list()` retruns the content of the web service request
#'     as a list.
#'
#' @examples
#' if (gcloud_exists())
#'     leonardo$getSystemStatus() %>% as.list()
#'
#' @export
as.list.response <-
    function(x, ..., as=c("text", "raw", "parsed"))
{
    as <- match.arg(as)
    value <- content(x, as=as, encoding = "UTF-8")
    if (identical(as, "text") && nzchar(value)) {
        value <- fromJSON(value)
    }
    value
}
