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


## Discover the binary based on a user defined path
.user_setting <- function(option) {
    val <- Sys.getenv(option, unset = NA)
    if (!is.na(val))
        val
    else
        NULL
}

## Get gsutil binary on user's machine for windows/linux/mac
.get_gsutil_binary <- function() {
    ## Path to find gsutil binary
    user_path <- .user_setting('GSUTIL_BINARY_PATH')
    if (!is.null(user_path))
        return(normalizePath(user_path))
    ## hardcode binary name (FIXME)
    binary_name <- 'gsutil'
    ## Discover binary automatically if user doesn't give path
    bin_path <-
        ## Windows machine
        if (.Platform$OS.type == "windows") {
            appdata <- normalizePath(
                Sys.getenv("localappdata"),
                winslash = "/")
            binary_name <- paste(binary_name, "cmd", sep = ".")
            c(function() {
                file.path(appdata,
                          "Google/Cloud SDK/google-cloud-sdk/bin",
                          binary_name)
            },
            function() {
                file.path(Sys.getenv("ProgramFiles"),
                          "/Google/Cloud SDK/google-cloud-sdk/bin",
                          binary_name)
            },
            function() {
                file.path(Sys.getenv("ProgramFiles(x86)"),
                          "/Google/Cloud SDK/google-cloud-sdk/bin",
                          binary_name)
            })
        } else {  ## If linux / mac
            c(function() {
                Sys.which(binary_name)
            },
            function() {
                paste("~/google-cloud-sdk/bin",
                      binary_name,
                      sep = "/")
            },
            function() {
                file.path(gcloud_binary_default(),
                                  "bin",
                          binary_name)
            })
        }
    ## Return appropriate path for 'gsutil'
    for (path in bin_path) {
        if (file.exists(path())) {
            return(normalizePath(path()))
        }
    }
    stop("failed to find 'gsutil' binary")
}


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
    gsutil <- .get_gsutil_binary()
    withCallingHandlers(tryCatch({
        system2(gsutil, args, stdout = TRUE, stderr = TRUE, wait=TRUE)
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


#' @rdname gsutil
#' 
#' @title `is_gsutil_uri()`: check if the google bucket has a gs://
#'     prefix.
#'
#' @param source character(1), a valid path to a google storage bucket.
#'
#' @return logical(1), is the source a  valid google cloud storage path?
#' 
is_gsutil_uri <-
    function(source)
{
    is.character(source) && grepl("^gs://.+$", source)
}


#' @rdname gsutil
#'
#' @title `gsutil_ls()`: List contents of a google cloud bucket, or if
#'     you run 'gsutil_ls' without URLs, it lists all of the Cloud
#'     Storage buckets under your default project ID
#'
#' @param source character(1), a valid path to a google storage bucket.
#'
#' @param path character(1), a path or regular expression listing
#'     files of paths.
#'
#' @param recursive logical(1), should the operation should be
#'     recursive?
#'
#' @return Exit status of `gsutil_ls()`, invisibly
#' 
gsutil_ls <-
    function(source = character(1), path = character(1),
             recursive = TRUE)
{
    stopifnot(
        .is_scalar_character(path, zchar = TRUE),
        .is_scalar_logical(recursive)
    )
    
    args <- c(
        "ls",
        if (recursive) "-r",
        file.path(if (nchar(source) > 0) source,
                  if (nchar(path) > 0) path)
    )
    .gsutil_do(args)
}


#' @rdname gsutil
#'
#' @title `gsutil_cp()`: copy contents of a gcs bucket.
#'
#' @param source character(1), a valid path to a google storage bucket.
#'
#' @param destination character(1), representing a destination local
#'     path.
#'
#' @param recursive logical(1), should the operation should be
#'     recursive?
#'
#' @return Exit status of `gsutil_cp()`, invisibly.
#'
#' @examples
#'
#' \dontrun{
#'    # for a single file
#'    gsutil_cp("gs://anvil-bioc/blah",
#'              "/tmp/blah-copy", recursive=FALSE, parallel=FALSE)
#'
#'    # for a folder with all contents
#'    # "/tmp/foobar-copy" must be an existing destination or must be created
#' 
#'    gsutil_cp("gs://anvil-bioc/foobar",
#'              "/tmp/foobar-copy", recursive=TRUE, parallel=TRUE)
#' }
#'
gsutil_cp <-
    function(source, destination,
             recursive = TRUE, parallel = TRUE)
{
    stopifnot(
        .is_scalar_character(source),
        .is_scalar_character(destination, zchar = TRUE),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel)
    )

    ## make sure source is valid google cloud storage location
    if (!is_gsutil_uri(source))
        stop("'source' must be a google cloud storage location with prefix 'gs://'.")

    args <- c(
        if (parallel) "-m", ## Makes the operations faster
        "cp", ## cp command
        if (recursive) "-r",
        source,
        destination
    )
    .gsutil_do(args)
    invisible(NULL)
}


#' @rdname gsutil
#'
#' @title `gsutil_stat()`: check if a bucket's subdirectory/file is
#'     present and get information regarding file.
#'
#' @param source character(1), a valid path to a google storage bucket.
#'
#' @return Exit status of `gsutil_stat()`, invisibly
#'
#' @examples
#'
#' \dontrun{
#'   gsutil_stat('gs://anvil-bioc', 'blah')
#'
#' gs://anvil-bioc/blah:
#' Creation time:          Tue, 11 Jun 2019 15:24:56 GMT
#' Update time:            Tue, 11 Jun 2019 15:24:56 GMT
#' Storage class:          STANDARD
#' Content-Language:       en
#' Content-Length:         7
#' Content-Type:           application/octet-stream
#' Metadata:
#'       goog-reserved-file-mtime:1560266597
#'   Hash (crc32c):          Ww1IQg==
#'   Hash (md5):             JGp1Xnjm4whB2lXhL3hziw==
#'   ETag:                   CInT4bHe4eICEAE=
#'   Generation:             1560266696845705
#'   Metageneration:         1
#' }
#'
gsutil_stat <-
    function(source)
{
    ## make sure source is valid google
    if (!is_gsutil_uri(source))
        stop("'source' must be a google cloud storage location with prefix 'gs://'.")

    ## make path
    path <- file.path(source)
    args <- c(
        "stat",
        path
    )
    .gsutil_do(args)
}


#' @rdname gsutil
#'
#' @title `gsutil_rm()`: remove contents of a google cloud bucket.
#'
#' @param source character(1), a valid path to a google storage bucket.
#' 
#' @param recursive logical(1), should the operation should be
#'     recursive?
#'
#' @param parallel logical(1), to perform parallel
#'     multi-threaded/multi-processing (default is TRUE).
#'
#' @return Exit status of `gsutil_rm()`, invisibly.
#'
#' @examples
#'
#' \dontrun{
#'    ## Remove a single file
#'    gsutil_rm('gs://anvil-bioc', 'blah', recursive = FALSE)
#'
#'    ## Remove a folder
#'    gsutil_rm('gs://anvil-bioc', 'foo-bar', recursive=TRUE, parallel=TRUE)
#' }
#'
gsutil_rm <-
    function(source, recursive = TRUE, parallel = TRUE)
{
    ## make sure source is valid google
    if (!is_gsutil_uri(source))
        stop("'source' must be a google cloud storage location with prefix 'gs://'.")

    ## remove
    args <- c(
        if (parallel) "-m",
        "rm",
        if (recursive) "-r",
        source
    )
    .gsutil_do(args)
}


#' @rdname gsutil
#' 
#' @title `gsutil_rsync()`: synchronize a source and a destination.
#' 
#' @param source character(1), a path to a source url, either a google
#'     bucket or a local path.
#'
#' @param destination character(1), a path to a destination url,
#'     either a google bucket or a local path.
#'
#' @param delete logical(1), a flag which matches the source to the
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
#' @param parallel logical(1), to perform parallel
#'     multi-threaded/multi-processing (default is TRUE).
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
#' @return Exit status of `gsutil_rsync()`, invisbly
#'
#' @examples
#'
#' \dontrun{
#'    ## Rsync from google bucket source to local destination
#'    gsutil_rsync('gs://anvil-bioc', '/tmp/local-copy')
#'
#'    ## Rsync from local source to google bucket
#'    gsutil_rsync('/tmp/local-copy', 'gs://anvil-bioc')
#' }
#' 
gsutil_rsync <-
    function(source, destination, delete = TRUE,
             recursive = TRUE, parallel = TRUE)
{
    ## if destination is not a google cloud repo, and does not exist
    if (!is_gsutil_uri(destination) && !dir.exists(destination))
        dir.create(destination)

    ## rsync operation
    args <- c(
        ##  -m option, to perform parallel (multi-threaded/multi-processing)
        if (parallel) "-m",
        "rsync",
        if (delete) "-d",
        if (recursive) "-r",
        source,
        destination
    )
    .gsutil_do(args)
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
