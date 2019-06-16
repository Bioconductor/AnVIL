#' @rdname localize
#'
#' @title Copy packages, folders, or files to or from google buckets.
#'
#' @description `localize()`: synchronize files from a Google storage
#'     bucket (`source`) to the local file system
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
#' @return `localize()`: exit status of function `gsutil_rsync()`.
#'
#' @examples
#'
#' \dontrun{
#'    localize("gs://anvil-bioc", "/tmp/test-localize")
#' }
#'
#' @export
localize <-
    function(source, destination)
{
    stopifnot(
        gsutil_is_uri(source),
        .is_scalar_character(destination), dir.exists(destination)
    )

    ## FIXME: return destination paths of copied files
    gsutil_rsync(source, destination, delete = FALSE, recursive = TRUE)
}

#' @rdname localize
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
#' @examples
#'
#' \dontrun{
#'     delocalize("/tmp/test-localize/", "gs://anvil-bioc")
#' }
#'
#' @export
delocalize <-
    function(source, destination, unlink = FALSE)
{
    stopifnot(
        .is_scalar_character(source), file.exists(source),
        gsutil_is_uri(destination)
    )
    ## sync and optionally remove source
    result <- gsutil_rsync(
        source, destination, delete = FALSE, recursive = TRUE
    )
    if (unlink)
        unlink(source, recursive=TRUE)
    result
}

#' @rdname localize
#'
#' @description `add_libpaths()`: Add local library paths to
#'     `.libPaths()`.
#'
#' @param paths `character()`: vector of directories to add to
#'     `.libPaths()`. Paths that do not exist will be created.
#'
#' @return `add_libpaths()`: updated .libPaths(), invisibly.
#'
#' @examples
#'
#' \dontrun{
#'    add_libpaths('/tmp/my_library')
#'    localize('gs://bioconductor-full-devel', '/tmp/my_library')
#' }
#'
#' @export
add_libpaths <-
    function(paths)
{
    stopifnot(is.character(paths))

    ## make sure all paths exist
    exist <- vapply(paths, dir.exists, logical(1))
    ok <- vapply(paths[!exist], dir.create, logical(1))
    if (!all(ok))
        stop(
            "'add_libpaths()' failed to create directories:\n",
            "  '", paste(paths[!exist][!ok], collapse="'\n  '"), "'"
        )

    .libPaths(c(paths, .libPaths()))
}

#' @importFrom utils available.packages installed.packages
.install_find_dependencies <-
    function(packages, lib)
{
    ## find dependencies
    db <- available.packages(repos = BiocManager::repositories())
    deps <- unlist(tools::package_dependencies(packages, db, recursive = TRUE),
                   use.names=FALSE)
    installed <- rownames(installed.packages(lib.loc = lib))
    unique(c(packages, deps[!deps %in% installed]))
}

## FIXME: buckets: don't use 'devel' for buckets, just version
## numbers; follow git convention for naming, e.g., RELEASE_3_10
.choose_google_bucket <-
    function()
{
    version <- sub(".", "_", BiocManager::version(), fixed=TRUE)
    paste0("gs://bioconductor-full-RELEASE_", version)
}

.package_exists <-
    function(google_bucket, pkgs = character())
{
    pkgs <- paste0(google_bucket, pkgs, sep="/")
    vapply(pkgs, gsutil_stat, logical(1))
}

#' @rdname localize
#'
#' @description `install()`: install packages and their dependencies
#'     from a bucket.
#'
#' @param pkgs `character()` packages to install from binary repository.
#'
#' @param lib `character(1)` library path (directory) in which to
#'     install `pkgs`; defaults to `.libPaths()[1]`.
#'
#' @param lib.loc `character()` library locations to search for
#'     currently installed packages. Default is all paths in
#'     `.libPaths()`.
#'
#' @return `install()`: location of installed packages, invisibly.
#'
#' @examples
#' \dontrun{
#' add_libpaths("/tmp/host-site-library")
#' install(packages = c('BiocParallel', 'BiocGenerics'))
#' }
#'
#' @export
install <-
    function(pkgs, lib = .libPaths()[1], lib.loc = NULL)
{
    ## Validate arguments
    stopifnot(is.character(pkgs), !anyNA(pkgs))

    packages <- .install_find_dependencies(pkgs, lib = lib.loc)
    ## copy from bucket to .libPaths()[1]
    ## Assumes packages are already in the google bucket

    google_bucket <- .choose_google_bucket()

    ## TODO: check if the package is in the bucket, if not throw error
    exist <- .package_exists(google_bucket, pkgs)
    if (!all(exist))
        stop(
            "package(s) do not exist:\n",
            "  '", paste(pkgs[!exist], collapse = "'\n  '" ), "'"
        )

    if (!gsutil_is_uri(google_bucket))
        stop("not a valid google bucket which exists in the GCP account")
    packages <- sprintf("%s/%s", google_bucket, packages)

    ## if there is more than one "source" i.e package(s)
    ## FIXME: gsutil_rsync should be vectorized, including 0-length source
    for (source in packages) {
        gsutil_rsync(
            source = source, destination = lib, delete = TRUE,
            recursive = TRUE
        )
    }
    invisible(file.path(lib, basename(packages)))
}
