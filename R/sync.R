## 3. AnVIL::sync(from, to)
##     1. Localize from(gs://) to (local/path)
##     2. Delocalize from(local/path) to (gs)
## 4. AnVIL::add_libs(from)
##     1. from(gs://)

## Helper function to run gsutil
.gsutil_do <-
    function(args)
{
    err <- system2("gsutil", args, wait=TRUE)
    if (err)
        stop(
            "\n", sprintf("gsutil %s", paste(args, collapse=" ")),
            "\n failed: error code ", err
        )
}


#' @rdname gsutil_cp
#'
#' @param google_bucket character, a valid path to a google storage
#'     buckte starting with "gs://"
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
        google_bucket, target_path
    )
    .gsutil_do(args)
}



#' @rdname gsutil_cp
#'
#' @param google_bucket character, a valid path to a google storage
#'     buckte starting with "gs://"
#'
#' @param sub_directory character vector, representing a sub directory
#'     path inside the google bucket
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @return Exit status of `gsutil_rm()`, invisibly
gsutil_rm <-
    function(google_bucket, sub_directory, recursive=TRUE)
{
    args <- c(
        "-m",
        "rm",
        if (recursive) "-r",
        file.path(google_bucket, subdirectory)
    )
}

## gsutil make bucket
## gsutil_mb <-
##     function()
## 



## to make gs://mybucket/data match the contents of
##  the local directory "data" you could do:
##     gsutil rsync -d data gs://mybucket/data
## To make the local directory "data" the same as the contents of
## gs://mybucket/data:
##    gsutil rsync -d -r gs://mybucket/data data

#' @rdname gsutil_rsync
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
#' 
gsutil_rsync <-
    function(source, destination, match=TRUE, recursive=TRUE)
{
    ## TODO: if destination is a google bucket
    if (!dir.exists(destination)) {
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
}


#' @rdname localize
#'
#' @title Localize a Google storage bucket to the container.
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket  starting with "gs://"
#'
#' @param local_path  character vector, representing a local path
#'
#' @return 
#'
#' @details
#'
#' @export
localize <-
    function(google_bucket, local_path, ...)
{   
    gsutil_rsync(google_bucket, local_path,...)
}

#' @rdname localize
#'
#' @title Delocalize a Google storage bucket from the container.
#'
#' @param local_path  character vector, representing a local path
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket  starting with "gs://"
#'
#' @return 
#'
#' @details
#'
#' @export
delocalize <-
    function(local_path, google_bucket, ...)
{
    gsutil_rsync(local_path, google_bucket, ...)
}


#' @rdname localize
#'
#' @title Add to .libPaths from a local path
#'
#' @param local_path charactor, identifying a local path
#'
#' @return character with latest .libPaths() 
#'
#' @details
#'
#' @export
add_libs <-
    function(local_path)
{       
    .libPaths(c(local_path, .libPaths())
}
