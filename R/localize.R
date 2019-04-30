#' @rdname localize
#'
#' @title Localize a Google storage bucket to the container.
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket.
#'
#' @param local_path character vector, representing a local path.
#'
#' @param ... additional arguments to `gsutil_rsync()`
#'
#' @return Exit status of function, `gsutil_rsync()`
#'
#' @details localize a bucket from the google cloud storage to a local
#'     path on the machine.
#'
#' @export
localize <-
    function(google_bucket, local_path, ...)
{
    gsutil_rsync(.gcs_pathify(google_bucket), local_path,...)
}

#' @rdname delocalize
#'
#' @title Delocalize a Google storage bucket from the container.
#'
#' @param local_path  character vector, representing a local path
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket.
#'
#' @return Exit status of function, `gsutil_rsync()`
#'
#' @details delocalize the volume which is mounted with the help of
#'     gsutil. This syncs the library a final time and then removes
#'     the volume from the directory. Once a path is delocalized, it
#'     has to be localized again to regain access.
#'
#' @export
delocalize <-
    function(local_path, google_bucket, remove_local_volume = TRUE, ...)
{
    ## First sync
    gsutil_rsync(local_path, .gcs_pathify(google_bucket), ...)
    ## then remove volume
    if (remove_local_volume) {
        unlink(local_path, recursive=TRUE)
    }
}


#' @rdname add_libs
#'
#' @title Add to .libPaths from a local path
#'
#' @param local_path character, identifying a local path
#'
#' @return character with latest .libPaths()
#'
#' @details Add the synced google bucket path(which is now local) to
#'     the library path .libPaths() so that R can see the hosted
#'     libraries.
#'
#' @export
add_libs <-
    function(local_path)
{
    ## Create directory if it doesn't exist
    ## otherwise .libPaths() doesn't work
    if (!dir.exists(local_path)) {
        dir.create(local_path)
    }
    .libPaths(c(local_path, .libPaths()))
}


#' @rdname sync
#'
#' @title Sync libraries from google bucket(source) and
#'     local_path(destination).
#'
#' @param source character, name of google bucket.
#'
#' @param destination character, represnting a local path.
#'
#' @details The sync function allows the user to sync
#'
#' @export
sync <-
    function(source, destination)
{
    if (.is_google_bucket(source)) {
        ## No need to .gcs_pathify as "localize" will call it.
        localize(google_bucket = source, local_path = destination)
    }
    ## Do not remove local volume
    if (.is_google_bucket(destination)) {
        delocalize(local_path = destination, google_bucket = source,
                   remove_local_volume = FALSE)
    }
}

.install_find_dependencies <-
    function(packages, lib)
{
    ## find dependencies
    db <- available.packages(repos = BiocManager::repositories())
    deps <- unlist(tools::package_dependencies(packages, db), use.names=FALSE)
    installed <- rownames(installed.packages(lib.loc = lib))
    unique(c(packages, deps[!deps %in% installed]))
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
    function(packages, google_bucket, lib = .libPaths()[1], lib.loc = NULL)
{
    packages <- .install_find_dependencies(packages, lib.loc = lib.loc)

    ## copy from bucket to .libPaths()[1]
    ## Assumes packages are already in the google bucket
    if (!.is_google_bucket(google_bucket))
        stop("not a valid google bucket which exists in the GCP account")
    packages <- sprintf("%s/%s", .gcs_pathify(google_bucket), packages)

    ## if there is more than one "source" i.e package(s)
    ## FIXME: gsutil_rsync should be vectorized, including 0-length source
    for (source in packages) {
        gsutil_rsync(
            source = source, desination = lib, match = TRUE,
            recursive = TRUE
        )
    }
    invisible(file.path(lib, basename(packages)))
}
