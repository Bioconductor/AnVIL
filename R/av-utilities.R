.get_platform <- function(default = "") {
    ## TODO: verify unique envvar in Azure workspaces
    opt <- .get_env_opt("WORKSPACE_ID", "AnVILAz.workspace_id", default)
    if (nzchar(opt))
        return("AnVILAz")

    opt <- .get_env_opt("GOOGLE_PROJECT", "GCLOUD_SDK_PATH", default)
    if (nzchar(opt))
        return("AnVILGCP")

    stop("The runtime environment must be within an AnVIL workspace.")
}

.get_env_opt <-
    function(envvar, option, default)
{
    opt <- Sys.getenv(envvar, unset = default)
    getOption(option, opt)
}

.check_pkg_avail <- function(package) {
    nzchar(system.file(package = package))
}

#' @name av-utilities
#'
#' @aliases avcopy avlist avremove
#'
#' @title List, copy, and remove files in either a Google Cloud Storage bucket
#'   or Azure Blob Storage container
#'
#' @description `avcopy()` copies files to and from either a Google Cloud
#'   Storage bucket or an Azure Blob Storage container using `gsutil` or
#'   `azcopy`, respectively. The function either calls the functions
#'   `gsutil_cp()` in the `AnVILGCP` package or `az_copy_to_storage()` /
#'   `az_copy_from_storage()` in the `AnVILAz` package.
#'
#' @param source `character(1)` a path to a Google Cloud Storage bucket or Azure
#'   Blob Storage container, possibly with wild-cards for file-level pattern
#'   matching.
#'
#' @param destination `character(1)`, Either a Google Cloud Storage bucket,
#'   Azure Blob Storage container, or a local file system destination path.
#'
#' @param \ldots additional arguments passed to `gsutil_cp()` or
#'   `az_copy_to_storage()` / `az_copy_from_storage()`
#'
#' @return For GCP functions, the exit status of function; otherwise, functions
#'   are called for the side effect of moving / removing files.
#'
#' @examples
#' if (AnVILGCP::gcloud_exists()) {
#'     src <- "gs://genomics-public-data/1000-genomes/other/sample_info/sample_info.csv"
#'     avcopy(src, tempdir())
#' } else if (AnVILAz::az_exists()) {
#'     src <- paste0(
#'         "https://curated1000genomes.blob.core.windows.net/dataset/nested/",
#'         "chr1/part-00002-tid-5326935979355877101-dd68248e-aa4b-419a-b1ce-",
#'         "fa2ccb6b0952-1006-1-c000.snappy.parquet"
#'     )
#'     avcopy(src, tempdir())
#' }
#'
#' @export
avcopy <- function(source, destination, ...) {
    stopifnot(
        isScalarCharacter(source),
        isScalarCharacter(destination)
    )
    platform <- .get_platform()

    if (identical(platform, "AnVILGCP") && .check_pkg_avail("AnVILGCP"))
        AnVILGCP::gsutil_cp(source, destination, ...)
    else if (identical(platform, "AnVILAz") && .check_pkg_avail("AnVILAz"))
        AnVILAz::az_copy(source, destination, ...)
    else
        stop("Install either 'AnVILGCP' or 'AnVILAz' for your workspace.")
}

#' @rdname av-utilities
#' @export
avlist <- function() {
    platform <- .get_platform()

    if (identical(platform, "AnVILGCP") && .check_pkg_avail("AnVILGCP"))
        AnVILGCP::gsutil_ls()
    else if (identical(platform, "AnVILAz") && .check_pkg_avail("AnVILAz"))
        AnVILAz::az_copy_list()
    else
        stop("Install either 'AnVILGCP' or 'AnVILAz' for your workspace.")
}

#' @rdname av-utilities
#' @export
avremove <- function(file, ...) {
    platform <- .get_platform()

    if (identical(platform, "AnVILGCP") && .check_pkg_avail("AnVILGCP"))
        AnVILGCP::gsutil_rm(source = file, ...)
    else if (identical(platform, "AnVILAz") && .check_pkg_avail("AnVILAz"))
        AnVILAz::az_copy_rm(blob_file = file)
    else
        stop("Install either 'AnVILGCP' or 'AnVILAz' for your workspace.")
}

#' @export
avbackup <- function(
    source,
    destination = "",
    ...
) {
    platform <- .get_platform()

    if (identical(platform, "AnVILGCP") && .check_pkg_avail("AnVILGCP"))
        AnVILGCP::avfiles_backup(
            source = source, destination = destination, ...
        )
    else if (identical(platform, "AnVILAz") && .check_pkg_avail("AnVILAz"))
        AnVILAz::az_copy_backup(
            from_dir = source, to_dir = destination, ...
        )
    else
        stop("Install either 'AnVILGCP' or 'AnVILAz' for your workspace.")
}

#' @export
avrestore <- function(
    source,
    destination = ".",
    ...
) {
    platform <- .get_platform()

    if (identical(platform, "AnVILGCP") && .check_pkg_avail("AnVILGCP"))
        AnVILGCP::avfiles_restore(
            source = source, destination = destination, ...
        )
    else if (identical(platform, "AnVILAz") && .check_pkg_avail("AnVILAz"))
        AnVILAz::az_copy_backup(
            from_dir = source, to_dir = destination, ...
        )
    else
        stop("Install either 'AnVILGCP' or 'AnVILAz' for your workspace.")
}
