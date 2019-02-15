##############################################
## TODO: Notes from martin
## 3. AnVIL::sync(from, to)
##     1. Localize from(gs://) to (local/path)
##     2. Delocalize from(local/path) to (gs)
## 4. AnVIL::add_libs(from)
##     1. from(gs://)
############################################

#' @title helper function to pass arguments to `gsutil`
#'
#' @param args arguments to helper function
#'
#' @details private helper function to pass arguments to `gsutil`
#'     command line utility and throw the appropriate errors.
#'
.gsutil_do <-
    function(args)
{
    err <- system2("gsutil", args, wait=TRUE)
    if (err) {
        stop(
            "\n", sprintf("gsutil %s", paste(args, collapse=" ")),
            "\n failed: error code ", err
        )
    }
}

## FIXME: How to suppress warnings
.is_google_bucket <-
    function(google_bucket)
{
    test <- try(
        gsutil_ls(google_bucket, recursive=FALSE),
        silent=TRUE
    )
    !inherits(test, "try-error")
}


.gcs_pathify <-
    function(google_bucket)
{
    idx <- !startsWith(google_bucket, "gs://")
    if (any(idx))
        google_bucket[idx] <- paste0("gs://", google_bucket[idx])
    google_bucket
}


#' @rdname gsutil_ls
#'
#' @title List contents of a gcs bucket
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket
#'
#' @param path character, a path or regular expression listing files
#'     of paths.
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @return Exit status of `gsutil_ls()`, invisibly
#'
gsutil_ls <-
    function(google_bucket, path="", recursive=TRUE)
{
    args <- c(
        "ls",
        if (recursive) "-r",
        file.path(.gcs_pathify(google_bucket), path)
    )
    .gsutil_do(args)
}



#' @rdname gsutil_cp
#'
#' @title Copy contents of a gcs bucket
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket
#'
#' @param local_path character vector, representing a local path
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @return Exit status of `gsutil_cp()`, invisibly
#'
gsutil_cp <-
    function(google_bucket, target_path ,recursive=TRUE)
{
    args <- c(
        "-m", ## Makes the operations faster
        "cp", ## cp command
        if (recursive) "-r",
        .gcs_pathify(google_bucket),
        target_path
    )
    .gsutil_do(args)
}



#' @rdname gsutil_rm
#'
#' @title Remove contents of a gcs bucket
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket
#'
#' @param sub_directory character vector, representing a sub directory
#'     path inside the google bucket
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @return Exit status of `gsutil_rm()`, invisibly
#'
gsutil_rm <-
    function(google_bucket, sub_directory, recursive=TRUE)
{
    args <- c(
        "-m",
        "rm",
        if (recursive) "-r",
        file.path(.gcs_pathify(google_bucket), subdirectory)
    )
    .gsutil_do(args)
}


#' @rdname gsutil_rsync
#'
#' @title rsync contents of a gcs bucket
#'
#' @param source character, a path to a source url. Either a google
#'     bucket or a local path
#'
#' @param destination character, a path to a destination url. Either a
#'     google bucket or a local path.
#'
#' @param match logical, a flag which matches the source to the
#'     destination exactly. (rsync -d)
#'
#'     The rsync -d option is very useful and commonly used, because
#'     it provides a means of making the contents of a destination
#'     bucket or directory match those of a source bucket or
#'     directory. However, please exercise caution when you use this
#'     option: It's possible to delete large amounts of data
#'     accidentally if, for example, you erroneously reverse source
#'     and destination.
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @details
#'
#' To make gs://mybucket/data match the contents of the local
#'     directory "data" you could do:
#'        `gsutil rsync -d data gs://mybucket/data`
#'
#' To make the local directory "data" the same as the contents of
#'    gs://mybucket/data:
#' `gsutil rsync -d -r gs://mybucket/data data`
#'
#' @return Exit status of `gsutil_rsync()`, invisbly
#'
gsutil_rsync <-
    function(source, destination, match=TRUE, recursive=TRUE)
{
    if (.is_google_bucket(source)) {
        source <- .gcs_pathify(source)
    }

    if (.is_google_bucket(destination)) {
        destination <- .gcs_pathify(destination)
    }

    ## if destination is not a google bucket and doesn't exist
    if (!dir.exists(destination) && (!.is_google_bucket(destination))) {
        dir.create(destination)
    }

    args <- c(
        ##  -m option, to perform parallel (multi-threaded/multi-processing)
        "-m",
        "rsync",
        if (match) "-d",
        if (recursive) "-r",
        source,
        destination
    )
    .gsutil_do(args)
}


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
