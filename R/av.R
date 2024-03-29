#' @name av-deprecated
#'
#' @title TABLE, DATA, files, bucket, runtime, and disk elements
NULL

##
## tables
##

.is_avtable <-
    function(table, namespace, name)
{
    tbls <- avtables(namespace, name)
    table %in% tbls$table
}

## helpers for avtable(), avtable_paged() avtable_import() handling
## treatment NA_character_
.avtable_na <-
    function(na)
{
    stopifnot(isCharacter(na, zchar = TRUE))
    function(x) {
        ## flatten() has changed "" (no attribute in the entity set)to
        ## NA_character_; change to ""
        x[is.na(x)] <- ""
        ## follow the rules of `na` re-assign NA_character_
        x[x %in% na] <- NA_character_
        x
    }
}

#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom dplyr bind_rows
.avtable_pages <-
    function(FUN, ..., n, page, pageSize)
{
    result <- NULL
    bar <- NULL
    repeat {
        response <- FUN(..., page = page, pageSize = pageSize)
        result <- bind_rows(result, response$results)
        if (is.null(bar)) {
            max <- max(min(n, response$resultMetadata$filteredCount), 1L)
            bar <- txtProgressBar(max = max, style = 3L)
            on.exit(close(bar))
        }
        setTxtProgressBar(bar, min(NROW(result), n))
        page <- response$parameters$page + 1L
        test <-
            (page > response$resultMetadata$filteredPageCount) ||
            (NROW(result) >= n)
        if (test)
            break
    }

    head(result, n)
}

#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode
#' @importFrom AnVILBase avstop_for_status
.avtable_paged1 <-
    function(
        namespace, name, table,
        page, pageSize, sortField, sortDirection,
        filterTerms, filterOperator)
{
    response <- Terra()$entityQuery(
        namespace, URLencode(name), table,
        page, pageSize, sortField, sortDirection,
        filterTerms, filterOperator)
    avstop_for_status(response, "avtable_paged")

    lst <-
        response %>%
        as.list()
    if (length(lst$results)) {
        results <- bind_cols(
            tibble(name = lst$results$name),
            as_tibble(lst$results$attributes)
        )
    } else {
        results <- tibble()
    }
    list(
        parameters = lst$parameters,
        resultMetadata = lst$resultMetadata,
        results = results
    )
}

#' @rdname av-deprecated
#'
#' @param table character(1) table name as returned by, e.g., `avtables()`.
#'
#' @param n numeric(1) maximum number of rows to return
#'
#' @param page integer(1) first page of iteration
#'
#' @param pageSize integer(1) number of records per page. Generally,
#'     larger page sizes are more efficient.
#'
#' @param sortField character(1) field used to sort records when
#'     determining page order. Default is the entity field.
#'
#' @param sortDirection character(1) direction to sort entities
#'     (`"asc"`ending or `"desc"`ending) when paging.
#'
#' @param filterTerms character(1) string literal to select rows with
#'     an exact (substring) matches in column.
#'
#' @param filterOperator character(1) operator to use when multiple
#'     terms in `filterTerms=`, either `"and"` (default) or `"or"`.
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @param na in `avtable()` and `avtable_paged()`, character() of
#'     strings to be interpretted as missing values. In
#'     `avtable_import()` character(1) value to use for representing
#'     `NA_character_`. See Details.
#'
#' @return `avtable_paged()`: a tibble of data corresponding to the
#'     AnVIL table `table` in the specified workspace.
#'
#' @importFrom BiocBaseUtils isScalarNumber isScalarInteger
#' @importFrom dplyr across where mutate
#' @import AnVILBase
#' @export
avtable_paged <-
    function(table,
        n = Inf, page = 1L, pageSize = 1000L,
        sortField = "name", sortDirection = c("asc", "desc"),
        filterTerms = character(),
        filterOperator = c("and", "or"),
        namespace = avworkspace_namespace(),
        name = avworkspace_name(), na = c("", "NA"))
{
    page <- as.integer(page)
    pageSize <- as.integer(pageSize)
    stopifnot(
        isScalarCharacter(table),
        isScalarNumber(n, infinite.ok = TRUE),
        isScalarInteger(page),
        isScalarInteger(pageSize),
        isScalarCharacter(sortField),
        length(filterTerms) == 0L || isScalarCharacter(filterTerms),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
        ## ,
        ## `unknown table; use 'avtables()' for valid names` =
        ##     .is_avtable(table, namespace, name)
    )
    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )
    sortDirection <- match.arg(sortDirection)
    filterOperator <- match.arg(filterOperator)
    name <- URLencode(name)
    na_fun <- .avtable_na(na)

    tbl <- .avtable_pages(
        .avtable_paged1,
        namespace = namespace, name = name, table = table,
        sortField = sortField, sortDirection = sortDirection,
        filterTerms = filterTerms, filterOperator = filterOperator,
        n = n, page = page, pageSize = pageSize
    )
    names(tbl) <- sub("^name$", paste0(table, "_id"), names(tbl))
    tbl %>%
        mutate(across(where(is.character), na_fun))
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

#' @importFrom utils write.table
.avtable_import_write_dataset <-
    function(.data, na)
{
    destination <- tempfile()
    write.table(
        .data, destination,
        quote = FALSE, sep = "\t", row.names=FALSE, na = na
    )

    destination
}

.avtable_import_pages_index <-
    function(.data, n, page, pageSize)
{
    ## import table in chunks to avoid server timeout;
    ## https://github.com/Bioconductor/AnVIL/issues/76

    ## arbitrary: use page size so that each 'chunk' is about 1M elements
    if (is.null(pageSize)) {
        N_ELEMENTS <- 1500000
        pageSize <- ifelse(
            prod(dim(.data)) > N_ELEMENTS,
            as.integer(floor(N_ELEMENTS / NCOL(.data))),
            NROW(.data)
        )
    }

    row_index <- seq_len(NROW(.data))
    ## assign all rows a page
    page_id <- (row_index - 1L) %/% pageSize + 1L
    ## exclude rows before `page`
    page_id[page_id < page] <- 0L
    ## return a maximum of `n` elements after the first non-zero element
    if (is.finite(n))
        page_id[row_index > sum(page_id == 0) + n] <- 0L

    pages <- split(row_index, page_id)
    pages <- pages[names(pages) != "0"]
    message("pageSize = ", pageSize, " rows (", length(pages), " pages)")
    pages
}

.avtable_import_chunks <-
    function(
        .data, namespace, name, delete_empty_values, na,
        n, page, pageSize
    )
{
    ## divide large tables into chunks, if necessary
    pages <- .avtable_import_pages_index(.data, n, page, pageSize)

    ## progress bar
    n_uploaded <- 0L
    progress_bar <- NULL
    if (length(pages) > 1L && interactive()) {
        progress_bar <- txtProgressBar(max = sum(lengths(pages)), style = 3L)
        on.exit(close(progress_bar))
    }

    status <- rep("Failed", length(pages))
    job_id <- rep(NA_character_, length(pages))
    ## iterate through pages
    for (chunk_index in seq_along(pages)) {
        chunk_idx <- pages[[chunk_index]]
        chunk_name <- names(pages)[[chunk_index]]
        chunk <- .data[chunk_idx, , drop = FALSE]
        job_id[[chunk_index]] <- tryCatch({
            .avtable_import(chunk, namespace, name, delete_empty_values, na)
        }, error = function(err) {
            msg <- paste(strwrap(paste0(
                "failed to import page ", chunk_name,
                "; continuing to next page"
            )), collapse = "\n")
            warning(msg, "\n", conditionMessage(err), immediate. = TRUE)
            NA_character_
        })
        n_uploaded <- n_uploaded + length(chunk_idx)
        if (!is.null(progress_bar))
            setTxtProgressBar(progress_bar, n_uploaded)
    }
    status[!is.na(job_id)] <- "Uploaded"
    tibble(
        page = seq_along(pages),
        from_row = vapply(pages, min, integer(1)),
        to_row = vapply(pages, max, integer(1)),
        job_id = job_id,
        status = status,
        message = rep(NA_character_, length(status))
    )
}

#' @importFrom httr content
.avtable_import <-
    function(.data, namespace, name, delete_empty_values, na)
{
    destination <- .avtable_import_write_dataset(.data, na)
    entities <- httr::upload_file(destination)

    response <- Terra()$flexibleImportEntities(
        namespace, URLencode(name),
        async = TRUE,
        deleteEmptyValues = delete_empty_values,
        entities = entities
    )
    avstop_for_status(response, "avtable_import")
    content(response)$jobId
}

#' @rdname av-deprecated
#'
#' @description `avtable_import_status()` queries for the status of an
#'     'asynchronous' table import.
#'
#' @param job_status tibble() of job identifiers, returned by
#'     `avtable_import()` and `avtable_import_set()`.
#'
#' @export
avtable_import_status <-
    function(job_status,
        namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        is.data.frame(job_status),
        c("job_id", "status") %in% colnames(job_status),
        isCharacter(job_status$job_id, na.ok = TRUE),
        isCharacter(job_status$status),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )
    todo <- !job_status$status %in% c("Done", "Failed")
    job_ids <- job_status$job_id[todo]
    n_jobs <- length(job_ids)
    updated_status <- rep(NA_character_, n_jobs)
    updated_message <- job_status$message
    names(updated_status) <- names(updated_message) <- job_ids

    progress_bar <- NULL
    message("checking status of ", n_jobs, " avtable import jobs")
    if (n_jobs > 1L && interactive()) {
        progress_bar <- txtProgressBar(max = n_jobs, style = 3L)
        on.exit(close(progress_bar))
    }

    for (job_index in seq_len(n_jobs)) {
        job_id <- job_ids[[job_index]]
        tryCatch({
            response <- Terra()$importJobStatus(namespace, name, job_id)
            avstop_for_status(response, "avtable_import_status")
            content <- httr::content(response)
            updated_status[[job_index]] <- content$status
            if ("message" %in% names(content)) {
                updated_message[[job_index]] <- gsub(
                    "[[:space:]]+", " ", content$message
                )
            } else {
                updated_message[[job_index]] <- NA_character_
            }
            if (identical(tolower(content$status),  "error"))
                stop(
                    "job failed with error status: ",
                    updated_message[[job_index]], call. = FALSE
                )
        }, error = function(err) {
            err_msg <- conditionMessage(err)
            is_err_status <- grepl("job failed with error status", err_msg)
            status_msg <- if (is_err_status) "error" else "failed to get"
            msg <- paste(strwrap(paste0(
                status_msg, " status of job_id '", job_id, "'; ",
                "continuing to next job"
            )), collapse = "\n")
            warning(msg, "\n", err_msg, immediate. = TRUE)
        })
        if (!is.null(progress_bar))
            setTxtProgressBar(progress_bar, job_index)
    }

    job_status$status[todo] <- updated_status
    job_status$message[todo] <- updated_message
    job_status
}

#' @rdname av-deprecated
#'
#' @description `avdata()` returns key-value tables representing the
#'     information visualized under the DATA tab, 'REFERENCE DATA' and
#'     'OTHER DATA' items.  `avdata_import()` updates (modifies or
#'     creates new, but does not delete) rows in 'REFERENCE DATA' or
#'     'OTHER DATA' tables.
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
#' library(AnVILBase)
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' ) {
#'     ## from within AnVIL
#'     data <- avdata()
#'     data
#' }
#'
#' @export
avdata <-
    function(namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    name <- URLencode(name)
    response <- Terra()$getWorkspace(
        namespace, URLencode(name), "workspace.attributes"
    )
    avstop_for_status(response, "avworkspace_data")

    content <- content(response)[[1]][["attributes"]]

    ## a workspace DATA element may be preceeded by the 'workspace:'
    ## tag, remove it
    names(content) <- sub("^workspace:", "", names(content))
    ## remove non-DATA attributes. `description` is from the workspace
    ## landing page. The `:` seems to be used as a delimiter, e.g.,
    ## `tag:tags`
    exclude <-
        names(content) %in% "description" |
        grepl("^[a-z]+:", names(content))
    content <- content[!exclude]

    ## some elements are lists, e.g., a vector of google
    ## buckets. Translate these to their character(1) representation,
    ## so the tibble has a column of type <chr> and shows the value of
    ## the character(1) entries, rather than a column of type list
    ## showing "chr(1)" for most elements
    is_character <- vapply(content, is.character, logical(1))
    content[!is_character] <- vapply(
        content[!is_character],
        ## list-like elements usually have a key-value structure, use
        ## the value
        function(x) jsonlite::toJSON(unlist(x[["items"]], use.names = FALSE)),
        character(1)
    )

    ## create the referenceData tibble; 'referenceData' keys start
    ## with "referenceData_"
    referenceData_id <- "referenceData_"
    referenceData_regex <- "^referenceData_([^_]+)_(.*)$"
    is_referenceData <- startsWith(names(content), referenceData_id)
    referenceData <- content[is_referenceData]
    referenceData_tbl <- tibble(
        type = rep("reference", length(referenceData)),
        table = sub(referenceData_regex, "\\1", names(referenceData)),
        key = sub(referenceData_regex, "\\2", names(referenceData)),
        value = as.character(unlist(referenceData, use.names = FALSE))
    )

    ## 'other' data
    otherData <- content[!is_referenceData]
    otherData_tbl <- tibble(
        type = "other",
        table = "workspace",
        key = names(otherData),
        value = as.character(unlist(otherData, use.names = FALSE))
    )

    bind_rows(otherData_tbl, referenceData_tbl)
}

#' @rdname av-deprecated
#'
#' @param .data A tibble or data.frame for import as an AnVIL table.
#'
#' @return `avdata_import()` returns, invisibly, the subset of the
#'     input table used to update the AnVIL tables.
#'
#' @examples
#' \dontrun{
#' avdata_import(data)
#' }
#'
#' @export
avdata_import <-
    function(
        .data, namespace = avworkspace_namespace(), name = avworkspace_name()
    )
{
    stopifnot(
        is.data.frame(.data),
        all(c("type", "table", "key", "value") %in% names(.data)),
        all(vapply(
            select(.data, "type", "table", "key", "value"),
            is.character,
            logical(1)
        )),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    .data <- filter(.data, .data$type == "other", table %in% "workspace")

    if (!nrow(.data)) {
        message(
            "'avdata_import()' has no rows of type 'other' and ",
            "table 'workspace'"
        )
        return(invisible(.data))
    }

    ## create a 'wide' table, with keys as column names and values as
    ## first row. Prefix "workspace:" to first column, for import
    keys <- paste0("workspace:", paste(.data$key, collapse = "\t"))
    values <- paste(.data$value, collapse = "\t")
    destination <- tempfile()
    writeLines(c(keys, values), destination)

    ## upload the table to AnVIL
    entities <- httr::upload_file(destination)
    response <- Terra()$importAttributesTSV(
        namespace, URLencode(name), entities
    )
    avstop_for_status(response, "avdata_import")

    invisible(.data)
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

#' @rdname av-deprecated
#'
#' @description `avbucket()` returns the workspace bucket, i.e., the
#'     google bucket associated with a workspace. Bucket content can
#'     be visualized under the 'DATA' tab, 'Files' item.
#'
#' @return `avbucket()` returns a `character(1)` bucket identifier,
#'     prefixed with `gs://` if `as_path = TRUE`.
#'
#' @examples
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' ) {
#'     ## From within AnVIL...
#'     bucket <- avstorage()                        # discover bucket
#' }
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
        isScalarCharacter(namespace),
        isScalarCharacter(name),
        isScalarLogical(as_path)
    )

    .life_cycle(
        newfun = "avstorage",
        newpackage = "AnVILGCP",
        title = "av"
    )

    if (.avbucket_cache$exists(namespace, name)) {
        bucket <- .avbucket_cache$get(namespace, name)
    } else {
        response <- Terra()$getWorkspace(
            namespace, URLencode(name), "workspace.bucketName"
        )
        avstop_for_status(response, "avbucket")
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

#' @rdname av-deprecated
#'
#' @description `avfiles_ls()` returns the paths of files in the
#'     workspace bucket.  `avfiles_backup()` copies files from the
#'     compute node file system to the workspace bucket.
#'     `avfiles_restore()` copies files from the workspace bucket to
#'     the compute node file system.  `avfiles_rm()` removes files or
#'     directories from the workspace bucket.
#'
#' @param path For `avfiles_ls(), the character(1) file or directory
#'     path to list. For `avfiles_rm()`, the character() (perhaps with
#'     length greater than 1) of files or directory paths to be
#'     removed. The elements of `path` can contain glob-style
#'     patterns, e.g., `vign*`.
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
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' )
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
        isScalarCharacter(path, zchar = TRUE),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
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

#' @rdname av-deprecated
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
        isScalarCharacter(destination, zchar = TRUE),
        isScalarLogical(recursive),
        isScalarLogical(parallel),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )

    bucket <- avbucket(namespace, name)
    destination <- .avbucket_path(bucket, destination)
    gsutil_cp(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av-deprecated
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
        isCharacter(source),
        isScalarCharacter(destination),
        `'destination' is not a directory` = dir.exists(destination),
        isScalarLogical(recursive),
        isScalarLogical(parallel),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, source)
    gsutil_cp(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av-deprecated
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
        isCharacter(source),
        isScalarLogical(recursive),
        isScalarLogical(parallel),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, source)
    result <- lapply(
        source, gsutil_rm, recursive = recursive, parallel = parallel
    )
    invisible(unlist(result))
}

##
## runtimes / persistent disks
##

#' @rdname av-deprecated
#'
#' @description `avruntimes()` returns a tibble containing information
#'     about runtimes (notebooks or RStudio instances, for example)
#'     that the current user has access to.
#'
#' @return `avruntimes()` returns a tibble with columns
#'
#' - id: integer() runtime identifier.
#' - googleProject: character() billing account.
#' - tool: character() e.g., "Jupyter", "RStudio".
#' - status character() e.g., "Stopped", "Running".
#' - creator character() AnVIL account, typically "user@gmail.com".
#' - createdDate character() creation date.
#' - destroyedDate character() destruction date, or NA.
#' - dateAccessed character() date of (first?) access.
#' - runtimeName character().
#' - clusterServiceAccount character() service ('pet') account for
#'   this runtime.
#' - masterMachineType character() It is unclear which 'tool' populates
#'   which of the machineType columns).
#' - workerMachineType character().
#' - machineType character().
#' - persistentDiskId integer() identifier of persistent disk (see
#'   `avdisks()`), or `NA`.
#'
#' @examples
#' if (gcloud_exists())
#'     ## from within AnVIL
#'     avruntimes()
#'
#' @importFrom dplyr rename_with
#'
#' @importFrom tidyselect everything
#'
#' @export
avruntimes <-
    function()
{
    template <- list(
        id = integer(0),
        googleProject = character(0),
        labels.tool = character(0),
        status = character(0),
        auditInfo.creator = character(0),
        auditInfo.createdDate = character(0),
        auditInfo.destroyedDate = logical(0),
        auditInfo.dateAccessed = character(0),
        runtimeName = character(0),
        labels.clusterServiceAccount = character(0),
        runtimeConfig.masterMachineType = character(0),
        runtimeConfig.workerMachineType = logical(0),
        runtimeConfig.machineType = character(0),
        runtimeConfig.persistentDiskId = integer(0)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )

    leo <- Leonardo()
    response <- leo$listRuntimes()
    avstop_for_status(response, "avruntimes")
    runtimes <- flatten(response)

    .tbl_with_template(runtimes, template) %>%
        rename_with(~ sub(".*\\.", "", .x))
}

#' @rdname av-deprecated
#'
#' @description `avruntime()` returns a tibble with the runtimes
#'     associated with a particular google project and account number;
#'     usually there is a single runtime satisfiying these criteria,
#'     and it is the runtime active in AnVIL.
#'
#' @param project `character(1)` project (billing account) name, as
#'     returned by, e.g., `gcloud_project()` or
#'     `avworkspace_namespace()`.
#'
#' @param account `character(1)` google account (email address
#'     associated with billing account), as returned by
#'     `gcloud_account()`.
#'
#' @return `avruntime()` returns a tibble witht he same structure as
#'     the return value of `avruntimes()`.
#'
#' @export
avruntime <-
    function(project = gcloud_project(), account = gcloud_account())
{
    stopifnot(
        isScalarCharacter(project),
        isScalarCharacter(account)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )

    rt <- avruntimes()
    rt %>%
        filter(.data$googleProject == project, .data$creator == account)
}

#' @importFrom dplyr pull
.runtime_pet <-
    function(creator, tool = c("Jupyter", "RStudio"),
             namespace = avworkspace_namespace())
{
    tool <- match.arg(tool)
    stopifnot(
        isScalarCharacter(tool),
        isScalarCharacter(creator),
        isScalarCharacter(namespace)
    )

    runtimes <- avruntimes()
    pet <-
        runtimes %>%
        filter(
            .data$tool == {{ tool }},
            .data$creator == {{ creator }},
            .data$googleProject == {{ namespace }}
        ) %>%
        pull(.data$clusterServiceAccount)

    if (!isScalarCharacter(pet))
        warning("'.runtime_pet' return value is not scalar")
    pet
}

#' @rdname av-deprecated
#'
#' @description 'avdisks()` returns a tibble containing information
#'     about persistent disks associatd with the current user.
#'
#' @return `avdisks()` returns a tibble with columns
#'
#' - id character() disk identifier.
#' - googleProject: character() billing account.
#' - status, e.g, "Ready"
#' - size integer() in GB.
#' - diskType character().
#' - blockSize integer().
#' - creator character() AnVIL account, typically "user@gmail.com".
#' - createdDate character() creation date.
#' - destroyedDate character() destruction date, or NA.
#' - dateAccessed character() date of (first?) access.
#' - zone character() e.g.. "us-central1-a".
#' - name character().
#'
#' @examples
#' if (gcloud_exists())
#'     ## from within AnVIL
#'     avdisks()
#'
#' @export
avdisks <-
    function()
{
    template <- list(
        id = integer(0),
        googleProject = character(0),
        status = character(0),
        size = integer(0),
        diskType = character(0),
        blockSize = integer(0),
        auditInfo.creator = character(0),
        auditInfo.createdDate = character(0),
        auditInfo.destroyedDate = logical(0),
        auditInfo.dateAccessed = character(0),
        name = character(0),
        zone = character(0)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        title = "av"
    )

    leo <- Leonardo()
    response <- leo$listDisks()
    avstop_for_status(response, "avdisks")
    runtimes <- flatten(response)

    .tbl_with_template(runtimes, template) %>%
        rename_with(~sub(".*\\.", "", .x))
}
