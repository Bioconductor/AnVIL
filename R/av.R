##
## internal
## 

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

##
## tables
##

#' Functions for convenient user interaction with AnVIL resources
#'
#' @rdname av
#'
#' @description `avtables()` describes tables available in a
#'     workspace. Tables can be visualized under the DATA tab, TABLES
#'     item.
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
#' @description `avdata()` returns key-value tables representing the
#'     information visualized under the DATA tab, 'REFERENCE DATA' and
#'     'OTHER DATA' items.
#'
#' @return `avdata()` returns a tibble with five columns: `"type"`
#'     represents the origin of the data from the 'REFERENCE' or
#'     'OTHER' data menus. `"table"` is the table name in the
#'     `REFERENCE` menu, or 'workspace' for the table in the 'OTHER'
#'     menu, the key used to access the data element, the value label
#'     associated with the data element and the value (e.g., google
#'     bucket) of the element.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
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

##
## workspace bucket
##

.avbucket_cache <- local({
    .key <- function(namespace, name)
        paste(namespace, name, sep = "/")
    buckets <- new.env(parent = emptyenv())

    list(exists = function(namespace, name) {
        exists(.key(namespace, name), envir = buckets)
    }, get = function(namespace, name) {
        buckets[[ .key(namespace, name) ]]
    }, set = function(namespace, name, bucket) {
        buckets[[ .key(namespace, name) ]] <- bucket
    }, keys = function() {
        names(buckets)
    }, flush = function() {
        rm(list = names(buckets), envir = buckets)
    })
})

#' @rdname av
#'
#' @description `avbucket()` returns the workspace bucket, i.e., the
#'     google bucket associated with a workspace. Bucket content can
#'     be visualized under the 'DATA' tab, 'Files' item.
#'
#' @return `avbucket()` returns a `character(1)` bucket identifier,
#'     prefixed with `gs://` if `as_path = TRUE`.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
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

    if (.avbucket_cache$exists(namespace, name)) {
        bucket <- .avbucket_cache$get(namespace, name)
    } else {
        name <- curl_escape(name)
        response <- Terra()$getWorkspace(namespace, name, "workspace.bucketName")
        .avstop_for_status(response, "avbucket")
        bucket <- as.list(response)$workspace$bucketName
        .avbucket_cache$set(namespace, name, bucket)
    }

    if (as_path)
        bucket <- paste0("gs://", bucket)
    bucket
}

.avbucket_path <-
    function(bucket, ...)
{
    stopifnot(.gsutil_is_uri(bucket))

    ## get path without duplicate "/"
    args <- expand.grid(..., stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    args <- unname(as.list(args))
    bucket <- sub("/*$", "", bucket)
    args <- lapply(args, sub, pattern = "^/*", replacement = "")
    args <- lapply(args, sub, pattern = "/*$", replacement = "")
    args <- do.call(
        "mapply",
        c(list(
            FUN = paste,
            MoreArgs = list(sep = "/"),
            USE.NAMES = FALSE
        ), args)
    )
    paste0(bucket, ifelse(length(args), "/", ""), args)
}

#' @rdname av
#'
#' @description `avfiles_ls()` returns the paths of files in the
#'     workspace bucket.
#'
#' @param path For `avfiles_ls(), the character(1) file or directory path
#'     to list.
#'
#'     For `avfiles_rm()`, the character() (perhaps with length
#'     greater than 1) of files or directory paths to be removed. The
#'     elements of `path` can contain glob-style patterns, e.g.,
#'     `vign*`.
#'
#' @param full_names logical(1) return names relative to `path`
#'     (`FALSE`, default) or root of the workspace bucket?
#'
#' @param recursive logical(1) list files recursively?
#'
#' @param as_path logical(1) when TRUE (default) return bucket with
#'     prefix `gs://` (for `avbucket()`) or `gs://<bucket-id>` (for
#'     `avfiles_ls()`).
#'
#' @return `avfiles_ls()` returns a character vector of files in the
#'     workspace bucket.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     avfiles_ls()
#'
#' @export
avfiles_ls <-
    function(
        path = "",
        full_names = FALSE,
        recursive = FALSE,
        namespace = avworkspace_namespace(), 
        name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(path, zchar = TRUE),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, path)
    result <- gsutil_ls(source, recursive = recursive)
    if (full_names) {
        sub(paste0(bucket, "/*"), "", result)
    } else {
        sub(paste0(source, "/*"), "", result)
    }
}

#' @rdname av
#'
#' @description `avfiles_backup()` copies files from the compute node
#'     file system to the workspace bucket.
#'
#' @details `avfiles_backup()` can be used to back-up individual files
#'     or entire directories, recursively.  When `recursive = FALSE`,
#'     files are backed up to the bucket with names approximately
#'     `paste0(destination, "/", basename(source))`.  When `recursive
#'     = TRUE` and source is a directory `path/to/foo/', files are
#'     backed up to bucket names that include the directory name,
#'     approximately `paste0(destination, "/", dir(basename(source),
#'     full.names = TRUE))`.  Naming conventions are described in
#'     detail in `gsutil_help("cp")`.
#'
#' @param source character() file paths. for `avfiles_backup()`,
#'     `source` can include directory names when `recursive = TRUE`.
#'
#' @param destination character(1) a google bucket
#'     (`gs://<bucket-id>/...`) to write files. The default is the
#'     workspace bucket.
#'
#' @param parallel logical(1) backup files using parallel transfer?
#'     See `?gsutil_cp()`.
#'
#' @return `avfiles_backup()` returns, invisibly, the status code of the
#'     `gsutil_cp()` command used to back up the files.
#'
#' @examples
#' \dontrun{
#' ## backup all files in the current directory
#' ## default buckets are gs://<bucket-id>/<file-names>
#' avfiles_backup(dir())
#' ## backup working directory, recursively
#' ## default buckets are gs://<bucket-id>/<basename(getwd())>/...
#' avfiles_backup(getwd(), recursive = TRUE)
#' }
#'
#' @export
avfiles_backup <-
    function(
        source,
        destination = "",
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        `some 'source' paths do not exist` = all(file.exists(source)),
        .is_scalar_character(destination, zchar = TRUE),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    bucket <- avbucket(namespace, name)
    destination <- .avbucket_path(bucket, destination)
    gsutil_cp(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av
#'
#' @description `avfiles_restore()` copies files from the workspace
#'     bucket to the compute node file system.
#'
#' @details `avfiles_restore()` behaves in a manner analogous to
#'     `avfiles_backup()`, copying files from the workspace bucket to
#'     the compute node file system.
#'
#' @export
avfiles_restore <-
    function(
        source,
        destination = ".",
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        .is_character(source),
        .is_scalar_character(destination),
        `'destination' is not a directory` = dir.exists(destination),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, source)
    gsutil_cp(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av
#'
#' @description `avfiles_rm()` removes files or directories from the
#'     workspace bucket.
#'
#' @return `avfiles_rm()` on success, returns a list of the return
#'     codes of `gsutil_rm()`, invisibly.
#'
#' @export
avfiles_rm <-
    function(
        source,
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )        
{
    stopifnot(
        .is_character(source),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, source)
    result <- lapply(
        source, gsutil_rm, recursive = recursive, parallel = parallel
    )
    invisible(unlist(result))
}

##
## utilities
##

.avworkspace <- local({
    hash <- new.env(parent = emptyenv())
    function(fun, key, value) {
        sysvar <- toupper(paste0("WORKSPACE_", key))
        if (is.null(value)) {
            if (is.null(hash[[key]])) {
                ## initialize
                hash[[key]] <- Sys.getenv(sysvar)
                if (!nzchar(hash[[key]]) && interactive())
                    warning("'", sysvar, "' undefined; use `", fun, "()` to set")
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
#'     from environment variables or interfaces usually available in
#'     AnVIL notebooks or RStudio sessions.
#'
#' @details `avworkspace_namespace()` is the billing account. If the
#'     `namespace=` argument is not provided, try `gcloud_project()`,
#'     and if that fails try `Sys.getenv("WORKSPACE_NAMESPACE")`.
#'
#'     `avworkspace_name()` is the name of the workspace as it appears
#'     in \url{https://app.terra.bio/#workspaces}. If not provided,
#'     `avworkspace_name()` tries to use
#'     `Sys.getenv("WORKSPACE_NAME")`.
#'
#'     Values are cached across sessions, so explicitly providing
#'     `avworkspace_*()` is required at most once per session. Revert
#'     to system settings with arguments `NA`.
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
avworkspace_namespace <- function(namespace = NULL) {
    suppressWarnings({
        namespace <- .avworkspace("avworkspace_namespace", "NAMESPACE", namespace)
    })
    if (!nzchar(namespace)) {
        namespace <- tryCatch({
            gcloud_project()
        }, error = function(e) {
            NULL
        })
        namespace <- .avworkspace("avworkspace_namespace", "NAMESPACE", namespace)
    }
    namespace
}

#' @rdname av
#'
#' @examples
#' avworkspace_name()
#'
#' @export
avworkspace_name <- function(name = NULL)
    .avworkspace("avworkspace_name", "NAME", name)
