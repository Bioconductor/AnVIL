#' @importFrom httr status_code http_condition
.avstop_for_status <-
    function(response, op)
{
    status <- status_code(response)
    if (status < 400L)
        return()

    cond <- http_condition(status, "error")
    msg <- as.list(response)[["message"]]
    message <- paste0(
        "'", op, "' failed:\n  ",
        conditionMessage(cond),
        if (!is.null(msg)) "\n  ", msg
    )
    stop(message, call.=FALSE)
}

#' Functions for convenient user interaction with AnVIL resources
#'
#' @rdname av
#' @export
avworkspace_namespace <- function()
    Sys.getenv('WORKSPACE_NAMESPACE')

#' @rdname av
#' @export
avworkspace_name <- function()
    Sys.getenv('WORKSPACE_NAME')

#' @rdname av
#' @export
avworkspace_bucket <- function()
    Sys.getenv('WORKSPACE_BUCKET', character(0))

#' @rdname av
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @return `avtables()`: A tibble with columns identifying the table,
#'     the number of records, and the column names.
#'
#' @export
avtables <-
    function(namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )
    name <- curl::curl_escape(name)
    types <- Terra()$getEntityTypes(namespace, name)
    .avstop_for_status(types, "avtables")
    lst <- content(types)
    table <- names(lst)
    count <- vapply(lst, `[[`, integer(1), "count")
    colnames <- vapply(lst, function(elt) {
        value <- unlist(elt[c("idName", "attributeNames")], use.names=FALSE)
        paste(value, collapse=", ")
    }, character(1))
    tibble(table, count, colnames)
}

#' @rdname av
#'
#' @param table character(1) table name as returned by, e.g., `avtables()`.
#'
#' @return `avtable()`: a tibble of data corresponding to the AnVIL
#'     table `table` in the specified workspace.
#'
#' @export
avtable <-
    function(table,
             namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(table),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )
    name = curl::curl_escape(name)
    entities <- Terra()$getEntities(namespace, name, table)
    .avstop_for_status(entities, "avtable")
    tbl <-
        as_tibble(flatten(entities)) %>%
        select(name, starts_with("attributes"), -ends_with("entityType"))
    names(tbl) <- sub("^attributes.", "", names(tbl))
    names(tbl) <- sub(".entityName$", "", names(tbl))
    tbl
}
