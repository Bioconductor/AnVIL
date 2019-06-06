#' @rdname gsutil
#'
#' @name gsutil
#'
#' @title Internal functions to interact with the gsutil command line
#'
#' @description These are internal (non-exported) helper functions to
#'     pass arguments to `gsutil` command line utility. Functions
#'     either succeed or return the appropriate error.
#'
#' @return All functions return the exit status of the corresponding
#'     `gsutil` command, invisibly.
NULL

.gsutil_do <-
    function(args)
{
    withCallingHandlers(tryCatch({
        system2("gsutil", args, stdout = TRUE, stderr = TRUE, wait=TRUE)
    }, error = function(err) {
        stop(
            "'gsutil ", paste(args, collapse = " "), "' failed:",
            "\n  ", conditionMessage(err),
            call. = FALSE
        )
    }), warning = function(warn) {
        warning(
            "'gsutil ", paste(args, collapse=" "), "' warning:",
            "\n  ", conditionMessage(warn),
            call. = FALSE
        )
        invokeRestart("muffleWarning")
    })
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
    google_bucket[idx] <- paste0("gs://", google_bucket[idx])
    google_bucket
}


#' @rdname gsutil
#'
#' @title `gsutil_ls()`: list contents of a gcs bucket.
#'
#' @param google_bucket character(1), a valid path to a google storage
#'     bucket. When missing, `gsutil_ls()` returns a vector of
#'     available buckets.
#'
#' @param path character(1), a path or regular expression to paths
#'     within the bucket.
#'
#' @param recursive logical(1), should the operation be recursive?
#'
gsutil_ls <-
    function(google_bucket = character(0), path = "", recursive = FALSE)
{
    stopifnot(
        .is_character_0_or_1(google_bucket),
        .is_scalar_character(path, zchar = TRUE),
        .is_scalar_logical(recursive)
    )
    args <- c(
        "ls",
        if (recursive) "-r",
        file.path(.gcs_pathify(google_bucket), path)
    )
    .gsutil_do(args)
}



#' @rdname gsutil
#'
#' @title `gsutil_cp()`: copy content between gcs buckets or between a
#'     bucket and a local file.
#'
gsutil_cp <-
    function(source, destination, recursive = FALSE)
{
    stopifnot(
        .is_scalar_character(google_bucket_path),
        .is_scalar_character(destination, zchar = TRUE),
        .is_scalar_logical(recursive)
    )
    args <- c(
        "-m", ## Makes the operations faster
        "cp", ## cp command
        if (recursive) "-r",
        .gcs_pathify(google_bucket_path),
        destination
    )
    result <- .gsutil_do(args)
    invisible(NULL)
}



#' @rdname gsutil
#'
#' @title `gsutil_rm()`: remove contents of a gcs bucket.
#'
#' @param sub_directory character(1), representing a sub directory
#'     path inside the google bucket.
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


#' @rdname gsutil
#'
#' @title `gsutil_rsync()`: synchronize a gcs bucket and local directory.
#'
#' @param source character(1), a path to a source url, either a google
#'     bucket or a local path
#'
#' @param destination character(1), a path to a destination url, either a
#'     google bucket or a local path.
#'
#' @param match logical(1), a flag which matches the source to the
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
#' @details
#'
#' To make `"gs://mybucket/data"` match the contents of the local
#' directory `"data"` you could do:
#'
#' \code{gsutil_rsync("data", "gs://mybucket/data", match = TRUE)}
#'
#' To make the local directory "data" the same as the contents of
#' gs://mybucket/data:
#'
#' \code{gsutil_rsync("gs://mybucket/data", "data", match = TRUE)}
#'
gsutil_rsync <-
    function(source, destination, match=TRUE, recursive=TRUE)
{
    if (.is_google_bucket(source))
        source <- .gcs_pathify(source)

    if (.is_google_bucket(destination))
        destination <- .gcs_pathify(destination)

    ## if destination is not a google bucket and doesn't exist
    if (!dir.exists(destination) && (!.is_google_bucket(destination)))
        dir.create(destination)

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
