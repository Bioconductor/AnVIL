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
#' @importFrom stats setNames
#'
#' @export
empty_object <- setNames(list(), character())

#' @rdname Services
#'
#' @param ... additional arguments passed to methods or, for
#'     `operations,Service-method`, to the internal `get_operation()`
#'     function.
#'
#' @param .deprecated optional logical(1) include deprecated operations?
#'
#' @export
setGeneric(
    "operations",
    function(x, ..., .deprecated = FALSE)
        standardGeneric("operations"),
    signature = "x"
)

#' @export
setMethod(
    "operations", "Service",
    function(x, ..., .deprecated = FALSE)
{
    operations <- .api_get_operations(.api(x), ...)
    deprecated <- .operation_field(operations, "deprecated")
    keep <- .deprecated | !vapply(deprecated, isTRUE, logical(1))
    operations[keep]
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
#' @importFrom tibble tibble
#' @importFrom dplyr filter arrange
#'
#' @export
tags <-
    function(x, .tags, .deprecated = FALSE)
{
    operations <- operations(x, .deprecated = .deprecated)

    tags <- .operation_field(operations, "tags")
    null_idx <- vapply(tags, is.null, logical(1))
    tags[null_idx] <- NA_character_
    names(tags) <- trimws(names(tags))

    summary <- .operation_field(operations, "summary")
    null_idx <- vapply(summary, is.null, logical(1))
    summary[null_idx] <- list(NA_character_)
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
#' @details When using `$` to select a service, some arguments appear
#'     in 'body' of the REST request. Specify these using the
#'     `.__body__=` argument, as illustrated for
#'     `createBillingProjectFull()`, below.
#'
#' @examples
#' if (gcloud_exists()) {
#'     ## Arguments to be used as the 'body' (`.__body__=`) of a REST query
#'     terra <- Terra()
#'     terra$createBillingProjectFull       # 6 arguments...
#'     args(terra$createBillingProjectFull) # ... passed as `.__body__ = list(...)`
#' }
#' @export
setMethod(
    "$", "Service",
    function(x, name)
{
    operation <- operations(x, .deprecated = TRUE)[name]
    if (isTRUE(.operation_field(operation, "deprecated")[[name]]))
        warning("'", name, "()' is deprecated")
    operation[[name]]
})
