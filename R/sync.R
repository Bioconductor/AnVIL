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
    paste0("gs://", google_bucket)
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
    function(local_path, google_bucket, ...)
{
    ## First sync
    gsutil_rsync(local_path, .gcs_pathify(google_bucket), ...)
    ## then remove volume
    unlink(local_path, recursive=TRUE)
}


#' @rdname add_libs
#'
#' @title Add to .libPaths from a local path
#'
#' @param local_path charactor, identifying a local path
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
    .libPaths(c(local_path, .libPaths()))
}
