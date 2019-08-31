#' @rdname Services
#'
#' @name Services
#'
#' @title RESTful services useful for AnVIL developers
#'
#' @aliases .DollarNames.Service operations,Service-method
#'     schemas,Service-method show,Service-method Service-class
NULL

#' @rdname Services
#'
#' @return `empty_object` returns a representation to be used as
#'     arguments in function calls expecting the empty json object
#'     `\{\}`.
#'
#' @format NULL
#'
#' @examples
#' empty_object
#'
#' @export
empty_object <- setNames(list(), character())

#' @rdname Services
#'
#' @export
setGeneric("operations", function(x) standardGeneric("operations"))

#' @export
setMethod(
    "operations", "Service",
    function(x)
{
    get_operations(.api(x))
})

#' @rdname Services
#'
#' @export
setGeneric("schemas", function(x) standardGeneric("schemas"))

#' @export
setMethod(
    "schemas", "Service",
    function(x)
{
    get_schemas(.api(x))
})

.operation_field <-
    function(operations, field)
{
    lapply(operations, function(operation) {
        definition <- attr(operation, "definition")
        definition[[field]]
    })
}

#' @rdname Services
#'
#' @param x A `Service` instance, usually a singleton provided by the
#'     package and documented on this page, e.g., `leonardo` or
#'     `terra`.
#'
#' @param .tags optional character() of tags to use to filter operations.
#'
#' @examples
#' terra <- Terra()
#' tags(terra)
#' tags(terra, "Billing")
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter arrange
#'
#' @export
tags <-
    function(x, .tags)
{
    operations <- operations(x)
    tags <- .operation_field(operations, "tags")
    null_idx <- vapply(tags, is.null, logical(1))
    tags[null_idx] <- NA_character_
    names(tags) <- trimws(names(tags))
    summary <- .operation_field(operations, "summary")
    summary <-  trimws(unlist(summary, use.names=FALSE))
    summary <- sub("\\\\", "", summary)
    tbl <- tibble(
        tag = unlist(tags, use.names=FALSE),
        operation = rep(names(tags), lengths(tags)),
        summary = rep(summary, lengths(tags))
    )
    if (!missing(.tags))
        tbl <- filter(tbl, tbl$tag %in% .tags)
    arrange(tbl, tbl$tag, tbl$operation)
}

#' @rdname Services
#'
#' @param name A symbol representing a defined operation, e.g.,
#'     `leonardo$listClusters()`.
#'
#' @export
setMethod(
    "$", "Service",
    function(x, name)
{
    operations(x)[[name]]
})
