.avnotebooks_runtime_path <-
    function(name)
{
    path.expand(file.path("~", name, "notebooks"))
}

.avnotebooks_workspace_path <-
    function(namespace, name)
{
    paste(avbucket(namespace, name), "notebooks", sep = "/")
}

#' @rdname avnotebooks
#' @md
#'
#' @title Notebook management
#'
#' @description `avnotebooks()` returns the names of the notebooks
#'     associated with the current workspace.
#'
#' @param local = `logical(1)` notebooks located on the workspace
#'     (`local = FALSE`, default) or runtime / local instance (`local
#'     = TRUE`). When `local = TRUE`, the notebook path is
#'     `<avworkspace_name>/notebooks`.
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @return `avnotebooks()` returns a character vector of buckets /
#'     files located in the workspace 'Files/notebooks' bucket path,
#'     or on the local file system.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     avnotebooks()
#'
#' @export
avnotebooks <-
    function(local = FALSE,
             namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        .is_scalar_logical(local),
        !local || (.is_scalar_character(namespace) && .is_scalar_character(name))
    )

    if (local) {
        dir(.avnotebooks_runtime_path(name))
    } else {
        basename(gsutil_ls(.avnotebooks_workspace_path(namespace, name)))
    }
}

#' @rdname avnotebooks
#' @md
#'
#' @description `avnotebooks_localize()` synchronizes the content of
#'     the workspace bucket to the local file system.
#'
#' @param destination missing or character(1) file path to the local
#'     file system directory for synchronization. The default location
#'     is `~/<avworkspace_name>/notebooks`. Out-of-date local files
#'     are replaced with the workspace version.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `avnotebooks_localize()` returns the exit status of
#'     `gsutil_rsync()`.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     avnotebooks_localize()  # dry run
#'
#' @export
avnotebooks_localize <-
    function(destination,
             namespace = avworkspace_namespace(), name = avworkspace_name(),
             dry = TRUE)
{
    ## FIXME: localize to persistent disk independent of current location
    ## .avnotebooks_localize_runtime(source, name, runtime_name, dry)

    stopifnot(
        missing(destination) || .is_scalar_character(destination),
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(dry)
    )

    source <- .avnotebooks_workspace_path(namespace, name)
    if (missing(destination)) {
        destination = .avnotebooks_runtime_path(name)
        if (!dry && !dir.exists(destination))
            dir.create(destination, recursive = TRUE)
    }
    localize(source, destination, dry = dry)
}

#' @rdname avnotebooks
#' @md
#'
#' @description `avnotebooks_delocalize()` synchronizes the content of
#'     the notebook location of the local file system to the workspace
#'     bucket.
#'
#' @param source missing or character(1) file path to the local file
#'     system directory for synchronization. The default location is
#'     `~/<avworkspace_name>/notebooks`. Out-of-date local files are
#'     replaced with the workspace version.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `avnotebooks_delocalize()` returns the exit status of
#'     `gsutil_rsync()`.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     try(avnotebooks_delocalize())  # dry run, fails if no local resource
#'
#' @export
avnotebooks_delocalize <-
    function(source,
             namespace = avworkspace_namespace(), name = avworkspace_name(),
             dry = TRUE)
{
    stopifnot(
        missing(source) || .is_scalar_character(source),
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(dry)
    )

    if (missing(source))
        source <- .avnotebooks_runtime_path(name)
    destination <- .avnotebooks_workspace_path(namespace, name)
    delocalize(source, destination, dry = dry)
}
