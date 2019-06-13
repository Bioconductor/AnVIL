#' @rdname localize
#'
#' @title Localize a Google storage bucket to the container.
#'
#' @param source character, a valid path to a google storage
#'     bucket.
#'
#' @param destination character vector, representing a local path.
#'
#' @param ... additional arguments to `gsutil_rsync()`
#'
#' @return Exit status of function, `gsutil_rsync()`
#'
#' @details localize a bucket from the google cloud storage to a local
#'     path on the machine.
#'
#' @examples
#'
#' \dontrun{
#'    localize("gs://anvil-bioc", "/tmp/test-localize")
#' }
#'
#' @export
localize <-
    function(source, destination, ...)
{
    ## make sure source is valid google
    if (!is_gsutil_uri(source))
        stop("'source' must be a google cloud storage location with prefix 'gs://'.")
    ## rsync
    gsutil_rsync(source, destination, ...)
}


#' @rdname delocalize
#'
#' @title Delocalize a Google storage bucket from the container.
#'
#' @param source character vector, representing a local path
#'
#' @param destination character, a valid path to a google storage
#'     bucket.
#'
#' @param remove_local_volume logical, remove the local mount volume.
#'
#' @param ... arguments for gsutil_rsync like, delete, recursive,
#'     parallel.
#'
#' @return Exit status of function, `gsutil_rsync()`
#'
#' @details delocalize the volume which is mounted with the help of
#'     gsutil. This syncs the library a final time and then removes
#'     the volume from the directory. Once a path is delocalized, it
#'     has to be localized again to regain access.
#'
#' @examples
#'
#' \dontrun{
#'     delocalize("/tmp/test-localize/", "gs://anvil-bioc")
#' }
#'
#' @export
delocalize <-
    function(source, destination, remove_local_volume = FALSE, ...)
{
    ## validate google bucket
    if (!is_gsutil_uri(destination))
        stop("'destination' must be a valid google cloud storage location with prefix 'gs://'.")
    ## First sync
    gsutil_rsync(source, destination, ...)
    ## then remove volume
    if (remove_local_volume) {
        unlink(source, recursive=TRUE)
    }
}


#' @rdname add_libs
#'
#' @title Add to .libPaths from a local path
#'
#' @param path character, identifying a local path
#'
#' @return character with latest .libPaths()
#'
#' @details Add the synced google bucket path(which is now local) to
#'     the library path .libPaths() so that R can see the hosted
#'     libraries.
#'
#' @examples
#'
#' \dontrun{
#'    add_libs('/tmp/my_library')
#'    localize('gs://bioconductor-full-devel', '/tmp/my_library')
#' }
#'
#' @export
add_libs <-
    function(path)
{
    ## Create directory if it doesn't exist
    ## otherwise .libPaths() doesn't work
    if (!dir.exists(path)) {
        dir.create(path)
    }
    .libPaths(c(local_path, .libPaths()))
}


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


## TODO: write test cases for testing .choose_google_bucket
.choose_google_bucket <-
    function()
{
    version <- as.character(BiocManager::version())
    ## If the version is devel
    if (BiocManager:::isDevel())
        google_bucket <- "gs://bioconductor-full-devel"
    else {
        release_version <- sub(".", "-", version, fixed=TRUE)
        google_bucket <- paste0(
            "gs://bioconductor-full-release", release_version
        )
    }
    google_bucket
}

.package_exists <-
    function(google_bucket, pkgs = character())
{
    for (pkg in pkgs) {
        gsutil_stat(source = google_bucket, sub_directory = pkg)
    }
}


#' @rdname install
#'
#' @title Install libraries and it's dependencies from a bucket.
#'
#' @param packages character, names of packages to install from a
#'     bucket.
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket.
#'
#' @param lib character, file path to .libPaths()[1], primary location
#'     to install packages.
#'
#' @details Given a pre-existing google bucket with a host of
#'     bioconductor packages built on a specific matching version of R
#'     and Bioconductor, the user is able to install packages his
#'     container using the 'install()` function from the google
#'     bucket.
#'
#' eg:
#'
#' add_libs("/tmp/host-site-library")
#'
#' install(packages = c('BiocParallel', ='BiocGenerics'),
#'             google_bucket = "bioc_release_volume",
#'             lib = .libPaths()[1])
#'
#' @return character, location of packages installed invisibly
#'
#' @export
install <-
    function(pkgs = character(), lib = .libPaths()[1], lib.loc = NULL)
{
    ## Validate arguments
    stopifnot(is.character(pkgs), !anyNA(pkgs))

    packages <- .install_find_dependencies(pkgs, lib = lib.loc)
    ## copy from bucket to .libPaths()[1]
    ## Assumes packages are already in the google bucket

    google_bucket <- .choose_google_bucket()

    ## TODO: check if the package is in the bucket, if not throw error
    if (!.package_exists(google_bucket, pkgs))
        stop("package does not exist")

    if (!is_gsutil_uri(google_bucket))
        stop("not a valid google bucket which exists in the GCP account")
    packages <- sprintf("%s/%s", google_bucket, packages)

    ## if there is more than one "source" i.e package(s)
    ## FIXME: gsutil_rsync should be vectorized, including 0-length source
    for (source in packages) {
        gsutil_rsync(
            source = source, desination = lib, delete = TRUE,
            recursive = TRUE
        )
    }
    invisible(file.path(lib, basename(packages)))
}
