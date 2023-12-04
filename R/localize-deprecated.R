#' @rdname localize-deprecated
#' @name localize-deprecated
#' @aliases localize delocalize
#'
#' @title Copy packages, folders, or files to or from Google buckets
#'   (DEPRECATED).
#'
#' @description `localize()`: recursively synchronizes files from a
#'     Google storage bucket (`source`) to the local file system
#'     (`destination`). This command acts recursively on the `source`
#'     directory, and does not delete files in `destination` that are
#'     not in `source.
#'
#' @param source `character(1)`, a google storage bucket or local file
#'     system directory location.
#'
#' @param destination `character(1)`, a google storage bucket or local
#'     file system directory location.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `localize()`: exit status of function `gsutil_rsync()`.
#'
#' @export
localize <-
    function(source, destination, dry = TRUE)
{
    stopifnot(
        .gsutil_is_uri(source),
        .is_scalar_character(destination), dir.exists(destination),
        .is_scalar_logical(dry)
    )
    if (dry)
        warning("use 'dry = FALSE' to localize source / destination")

    .life_cycle(
        newpackage = "AnVILGCP", cycle = "deprecated", title = "localize"
    )
    ## FIXME: return destination paths of copied files
    gsutil_rsync(
        source, destination, delete = FALSE, recursive = TRUE, dry = dry
    )
}

#' @rdname localize-deprecated
#'
#' @description `delocalize()`: synchronize files from a local file
#'     system (`source`) to a Google storage bucket
#'     (`destination`). This command acts recursively on the `source`
#'     directory, and does not delete files in `destination` that are
#'     not in `source`.
#'
#' @param unlink `logical(1)` remove (unlink) the file or directory
#'     in `source`. Default: `FALSE`.
#'
#' @return `delocalize()`: exit status of function `gsutil_rsync()`
#'
#' @export
delocalize <-
    function(source, destination, unlink = FALSE, dry = TRUE)
{
    stopifnot(
        .is_scalar_character(source), file.exists(source),
        .gsutil_is_uri(destination),
        .is_scalar_logical(unlink),
        .is_scalar_logical(dry)
    )
    if (dry)
        warning("use 'dry = FALSE' to delocalize source / destination")

    .life_cycle(
        newpackage = "AnVILGCP", cycle = "deprecated", title = "localize"
    )
    ## sync and optionally remove source
    result <- gsutil_rsync(
        source, destination, delete = FALSE, recursive = TRUE, dry = dry
    )
    if (!dry && unlink)
        unlink(source, recursive=TRUE)
    result
}
