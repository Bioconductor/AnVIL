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
#'
#' @description `avtables()` describes tables available in a
#'     workspace.
#'
#' @return `avtables()`: A tibble with columns identifying the table,
#'     the number of records, and the column names.
#'
#' @importFrom curl curl_escape
#'
#' @export
avtables <-
    function(namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )
    name <- curl_escape(name)
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
#' @description `avtable()` returns an AnVIL table.
#'
#' @param table character(1) table name as returned by, e.g., `avtables()`.
#'
#' @return `avtable()`: a tibble of data corresponding to the AnVIL
#'     table `table` in the specified workspace.
#'
#' @importFrom dplyr "%>%" select starts_with ends_with
#'
#' @export
avtable <-
    function(table, namespace = avworkspace_namespace(),
        name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(table),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )
    name = curl_escape(name)
    entities <- Terra()$getEntities(namespace, name, table)
    .avstop_for_status(entities, "avtable")
    tbl <-
        as_tibble(flatten(entities)) %>%
        select(name, starts_with("attributes"), -ends_with("entityType"))
    names(tbl) <- sub("^attributes.", "", names(tbl))
    names(tbl) <- sub(".entityName$", "", names(tbl))
    tbl
}

.avtable_import_set_entity <-
    function(.data, entity)
{
    oentity <- entity
    idx <- match(entity, names(.data))

    if (!startsWith(entity, "entity:"))
        entity <- paste0("entity:", entity)
    if (!endsWith(entity, "_id"))
        entity <- paste0(entity, "_id")

    if (is.na(idx)) {                   # new column, arbitrary index
        .data[[entity]] <- seq_len(nrow(.data))
    } else {                           # existing column, maybe rename
        names(.data)[idx] <- entity
    }

    stopifnot(!anyDuplicated(.data[[entity]]), !anyNA(.data[[entity]]))

    .data[c(entity, setdiff(names(.data), entity))]
}

#' @rdname av
#'
#' @description `avtable_import()` imports a data.frame to an AnVIL table.
#'
#' @param .data A tibble or data.frame for import as an AnVIL table.
#'
#' @param entity `character(1)` column name of `.data` to be used as
#'     imported table name. When the table comes from R, this is
#'     usually a column name such as `sample`. The data will be
#'     imported into AnVIL as a table `sample`, with the `sample`
#'     column included with suffix `_id`, e.g., `sample_id`. A column
#'     in `.data` with suffix `_id` can also be used, e.g., `entity =
#'     "sample_id"`, creating the table `sample` with column
#'     `sample_id` in AnVIL. Finally, a value of `entity` that is not
#'     a column in `.data`, e.g., `entity = "unknown"`, will cause a
#'     new table with name `entity` and entity values
#'     `seq_len(nrow(.data))`.
#'
#' @return `avtable_import()` returns a `character(1)` name of the
#'     imported AnVIL tibble.
#'
#' @importFrom utils write.table
#'
#' @export
avtable_import <-
    function(.data, entity = names(.data)[[1]],
        namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        is.data.frame(.data),
        .is_scalar_character(entity),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    name = curl_escape(name)

    .data <- .avtable_import_set_entity(.data, entity)
    destination <- tempfile()
    write.table(.data, destination, quote = FALSE, sep="\t", row.names=FALSE)

    entities <- httr::upload_file(destination)
    response <- Terra()$flexibleImportEntities(namespace, name, entities)
    .avstop_for_status(response, "avtable_import")

    httr::content(response, type="text", encoding = "UTF-8")
}

#' @rdname av
#'
#' @description `avtable_delete_values()` removes rows from an AnVIL table.
#'
#' @param values vector of values in the entity (key) column of
#'     `table` to be deleted. A table `sample` has an associated
#'     entity column with suffix `_id`, e.g., `sample_id`. Rows with
#'     entity column entries matching `values` are deleted.
#'
#' @return `avtable_delete_values()` returns a `tibble` representing
#'     deleted entities, invisibly.
#'
#' @export
avtable_delete_values <-
    function(table, values,
        namespace = avworkspace_namespace(),
        name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(table),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    name <- curl_escape(name)
    body <- tibble(entityType = table, entityName = as.character(values))

    response <- Terra()$deleteEntities(namespace, name, body)
    .avstop_for_status(response, "avtable_delete_values")

    invisible(body)
}

#' @rdname av
#'
#' @description `avdata()` returns the key-value table of 'REFERENCE
#'     DATA' and 'OTHER DATA' workspace attributes.
#'
#' @return `avdata()` returns a tibble with five columns: `"type"`
#'     represents the origin of the data from the 'REFERENCE' or
#'     'OTHER' data menus. `"table"` is the table name in the
#'     `REFERENCE` menu, or 'workspace' for the table in the 'OTHER'
#'     menu, the key used to access the data element, the value label
#'     associated with the data element and the value (e.g., google
#'     bucket) of the element.
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_namespace()))
#'     ## from within AnVIL
#'     avdata()
#'
#' @export
avdata <-
    function(namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    name <- curl_escape(name)
    response <- Terra()$exportAttributesTSV(namespace, name)
    .avstop_for_status(response, "avworkspace_data")

    content <- content(response)
    lines <- unlist(strsplit(content, "\n"))
    fields <- strsplit(lines, "\t")

    field1 <- sub("workspace:", "", fields[[1]])
    is_referenceData <- startsWith(field1, "referenceData")
    type <- ifelse(is_referenceData, "reference", "other")
    table <- rep("workspace", length(field1))
    table[is_referenceData] <- vapply(
        strsplit(field1[is_referenceData], "_"), `[[`, character(1), 2L
    )
    key <- sub("^referenceData_[^_]+_", "", field1)

    tbl <- tibble(
        type = type, table = table, key = key,
        label = basename(fields[[2]]),
        value = fields[[2]]
    )
    arrange(tbl, type, table, key)
}

#' @rdname av
#'
#' @description `avbucket()` retrieves the google bucket associated
#'     with a workspace.
#'
#' @param as_path logical(1) when TRUE (default) return bucket with
#'     prefix `gs://`.
#'
#' @return `avbucket()` returns a `character(1)` bucket identifier,
#'     prefixed with `gs://` if `as_path = TRUE`.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_namespace()))
#'     ## From within AnVIL...
#'     bucket <- avbucket()                        # discover bucket
#'
#' \dontrun{
#' path <- file.path(bucket, "mtcars.tab")
#' gsutil_ls(dirname(path))                    # no 'mtcars.tab'...
#' write.table(mtcars, gsutil_pipe(path, "w")) # write to bucket
#' gsutil_stat(path)                           # yep, there!
#' read.table(gsutil_pipe(path, "r"))          # read from bucket
#' }
#' @export
avbucket <-
    function(namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        as_path = TRUE)
{
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(as_path)
    )

    name <- curl_escape(name)
    response <- Terra()$getWorkspace(namespace, name, "workspace.bucketName")
    .avstop_for_status(response, "avbucket")

    bucket <- as.list(response)$workspace$bucketName
    if (as_path)
        bucket <- paste0("gs://", bucket)
    bucket
}

.avworkspace <- local({
    hash <- new.env(parent = emptyenv())
    function(key, value) {
        sysvar <- toupper(paste0("WORKSPACE_", key))
        if (is.null(value)) {
            if (is.null(hash[[key]])) {
                ## initialize
                hash[[key]] <- Sys.getenv(sysvar)
                if (!nzchar(hash[[key]]) && interactive())
                    warning("'", key, "' undefined; use `", key, "()` to set")
            }
        } else {
            hash[[key]] <- ifelse(is.na(value), Sys.getenv(sysvar), value)
        }
        hash[[key]]
    }
})

#' @rdname av
#'
#' @description `avworkspace_namespace()` and `avworkspace_name()` are
#'     utiliity functions to retrieve workspace namespace and name
#'     from environment variables or interfaces available in
#'     AnVIL. The `namespace` is usually the billing account, and the
#'     `name` is the name of the workspace as it appears in
#'     \url{https://app.terra.bio/#workspaces}. Providing arguments to
#'     these functions over-rides AnVIL-determined settings with the
#'     provided value. Revert to system settings with arguments `NA`.
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @return `avworkspace_namespace()`, and `avworkspace_name()` return
#'     `character(1)` identifiers.
#'
#' @examples
#' avworkspace_namespace()
#'
#' @export
avworkspace_namespace <- function(namespace = NULL)
    .avworkspace("avworkspace_namespace", namespace)

#' @rdname av
#'
#' @examples
#' avworkspace_name()
#'
#' @export
avworkspace_name <- function(name = NULL)
    .avworkspace("avworkspace_name", name)
