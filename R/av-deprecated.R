#' @name av-deprecated
#'
#' @aliases avbucket avfiles_ls avfiles_backup avfiles_restore avfiles_rm
#'
#' @title Deprecated higher level functions that use `gsutil`
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
        response <- Terra()$getWorkspace(
            namespace, URLencode(name), "workspace.bucketName"
        )
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
        .is_scalar_character(destination, zchar = TRUE),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .Deprecated(
        msg = c(
            "'avfiles_backup' is deprecated.\n",
            "Use the 'avbackup' instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
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
        .is_character(source),
        .is_scalar_character(destination),
        `'destination' is not a directory` = dir.exists(destination),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .Deprecated(
        msg = c(
            "'avfiles_restore' is deprecated.\n",
            "Use the 'avrestore' instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
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
        .is_character(source),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .Deprecated(
        msg = c(
            "'avfiles_rm' is deprecated.\n",
            "Use the 'avremove' instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, source)
    result <- lapply(
        source, gsutil_rm, recursive = recursive, parallel = parallel
    )
    invisible(unlist(result))
}
