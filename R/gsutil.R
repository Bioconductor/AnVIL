#' @rdname gsutil
#'
#' @name gsutil
#'
#' @title Internal functions to interact with the gsutil command line
#'
#' @description These are internal (non-exported) functions that
#'     invoke the `gsutil` command line utility.
#'
#' @details The `gsutil` system command is required.  The search for
#'     `gsutil` starts with environment variable `GSUTIL_BINARY_PATH`
#'     providing the full path to the binary.  On Windows, the search
#'     tries to find `Google\\Cloud
#'     SDK\\google-cloud-sdk\\bin\\gsutil.cmd` in the `LOCAL APP
#'     DATA`, `Program Files`, and `Program Files (x86)` directories.
#'     On linux / macOS, the search continues with `Sys.which()`,
#'     followed by `GCLOUD_INSTALL_PATH` (such that `gsutil` is
#'     located at `$GCLOUD_INSTALL_PATH/bin/gsutil`), and defaulting
#'     to `~/google-cloud-sdk/bin/gsutil`.
NULL

## environment variable or NULL, allowing for default `unset` value
.gsutil_getenv <-
    function(option, unset = NA)
{
    value <- Sys.getenv(option, unset = unset)
    if (is.na(value))
        NULL
    else
        value
}

## Get gsutil binary on user's machine for windows/linux/mac
.gsutil_find_binary <-
    function()
{
    ## Path to find gsutil binary
    user_path <- .gsutil_getenv('GSUTIL_BINARY_PATH')
    if (!is.null(user_path))
        return(normalizePath(user_path))
    ## hardcode binary name (FIXME)
    binary_name <- 'gsutil'
    ## Discover binary automatically if user doesn't give path
    bin_path <-
        if (.Platform$OS.type == "windows") {
            gsk_binary_path <- file.path(
                "Google", "Cloud SDK", "google-cloud-sdk", "bin",
                paste(binary_name, "cmd", sep = ".")
            )
            c(function() {
                appdata <- normalizePath(
                    Sys.getenv("localappdata"), winslash = "/"
                )
                file.path(appdata, gsk_binary_path)
            },
            function() {
                file.path(Sys.getenv("ProgramFiles"), gsk_binary_path)
            },
            function() {
                file.path(Sys.getenv("ProgramFiles(x86)"), gsk_binary_path)
            })
        } else {                        # linux / mac
            c(function() {
                Sys.which(binary_name)
            },
            function() {
                gcloud_install_path <-
                    .gsutil_getenv("GCLOUD_INSTALL_PATH", "~/google-cloud-sdk")
                file.path(gcloud_install_path, "bin", binary_name)
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

## evaluate the gsutil command and arguments in `args`
.gsutil_do <-
    function(args)
{
    gsutil <- .gsutil_find_binary()
    wmsg <- NULL
    value <- withCallingHandlers(tryCatch({
        system2(gsutil, args, stdout = TRUE, stderr = TRUE, wait=TRUE)
    }, error = function(err) {
        msg <- paste0(
            "'gsutil ", paste(args, collapse = " "), "' failed:\n",
            "  ", conditionMessage(err)
        )
        stop(msg, call. = FALSE)
    }), warning = function(warn) {
        wmsg <<- c(wmsg, conditionMessage(warn))
        invokeRestart("muffleWarning")
    })
    if (!is.null(attr(value, "status"))) {
        warning(
            "'gsutil ", paste(args, collapse = " "), "'\n",
            "  exit status: ", attr(value, "status"), "\n",
            "  value: ", as.vector(value),
            call. = FALSE
        )
        value <- invisible(NULL)
    }
    value
}

#' @rdname gsutil
#' 
#' @description `gsutil_is_uri()` returns TRUE if the `source`
#'     argument has a `gs://` prefix.
#'
#' @param source `character(1)`, paths to a google storage bucket,
#'     possibly with wild-cards for file-level pattern matching.
#'
#' @return `gsutil_is_uri()`: a logical vector of length equal to
#'     `source`, with `TRUE` values indicating that the `source` is a
#'     character vector with corresponding element prefix `gs://`.
gsutil_is_uri <-
    function(source)
{
    is.character(source) & startsWith(source, "gs://")
}

#' @rdname gsutil
#'
#' @description `gsutil_ls()`: List contents of a google cloud bucket
#'     or, if `source` is missing, all Cloud Storage buckets under
#'     your default project ID
#'
#' @param recursive `logical(1)`; perform operation recursively from
#'     `source`?. Default: `FALSE`.
#'
#' @return `gsutil_ls()`: exit status of `gsutil_ls()`, invisibly.
gsutil_ls <-
    function(source = character(), recursive = FALSE)
{
    stopifnot(
        .is_scalar_character(source, zchar = TRUE),
        .is_scalar_logical(recursive)
    )
    
    args <- c(
        "ls",
        if (recursive) "-r",
        source
    )
    .gsutil_do(args)
}

#' @rdname gsutil
#'
#' @description `gsutil_cp()`: copy contents of `source` to
#'     `destination`. At least one of `source` or `destination` must
#'     be Google cloud bucket; `source` can be a character vector with
#'     length greater than 1. Use `gsutil_help("cp")` for `gsutil` help.
#'
#' @param destination `character(1)`, google cloud bucket or local
#'     file system destination path.
#'
#' @param parallel `logical(1)`, perform parallel multi-threaded /
#'     multi-processing (default is `TRUE`).
#'
#' @return `gsutil_cp()`: exit status of `gsutil_cp()`, invisibly.
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
    function(source, destination, recursive = FALSE, parallel = TRUE)
{
    stopifnot(
        .is_scalar_character(source), .is_scalar_character(destination),
        gsutil_is_uri(source) || gsutil_is_uri(destination),
        .is_scalar_logical(recursive), .is_scalar_logical(parallel)
    )

    args <- c(
        if (parallel) "-m", ## Makes the operations faster
        "cp", ## cp command
        if (recursive) "-r",
        source,
        destination
    )
    .gsutil_do(args)
}

#' @rdname gsutil
#'
#' @description `gsutil_stat()`: status of a bucket, directory, or file.
#'
#' @return `gsutil_stat()`: exit status of `gsutil_stat()`, invisibly.
#'
#' @examples
#'
#' \dontrun{
#'     gsutil_stat('gs://anvil-bioc', 'blah')
#' }
#'
gsutil_stat <-
    function(source)
{
    stopifnot(gsutil_is_uri(source))

    ## make path
    path <- file.path(source)
    args <- c("stat", path)
    .gsutil_do(args)
}

#' @rdname gsutil
#'
#' @description `gsutil_rm()`: remove contents of a google cloud
#'     bucket.
#'
#' @return `gsutil_rm()`: exit status of `gsutil_rm()`, invisibly.
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
    function(source, recursive = FALSE, parallel = TRUE)
{
    stopifnot(gsutil_is_uri(source))

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
#' @description `gsutil_rsync()`: synchronize a source and a destination.
#' 
#' @param delete `logical(1)`, when `TRUE`, remove files in
#'     `destination` that are not in `source`. Exercise caution when
#'     you use this option: it's possible to delete large amounts of
#'     data accidentally if, for example, you erroneously reverse
#'     source and destination.
#'
#' @details
#'
#' `gsutil_rsync()': To make `"gs://mybucket/data"` match the contents
#' of the local directory `"data"` you could do:
#'
#' \code{gsutil_rsync("data", "gs://mybucket/data", delete = TRUE)}
#'
#' To make the local directory "data" the same as the contents of
#' gs://mybucket/data:
#'
#' \code{gsutil_rsync("gs://mybucket/data", "data", delete = TRUE)}
#'
#' If `destination` is a local path and does not exist, it will be
#' created.
#'
#' @return `gsutil_rsync()`: exit status of `gsutil_rsync()`, invisbly.
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
    function(source, destination, delete = FALSE, recursive = FALSE,
             parallel = TRUE)
{
    stopifnot(
        .is_scalar_character(source), .is_scalar_character(destination),
        gsutil_is_uri(source) || gsutil_is_uri(destination),
        .is_scalar_logical(delete),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel)
    )
    ## if destination is not a google cloud repo, and does not exist
    if (!gsutil_is_uri(destination) && !dir.exists(destination))
        if (!dir.create(destination))
            stop("'gsutil_rsync()' failed to create '", destination, "'")

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

#' @rdname gsutil
#'
#' @description `gsutil_help()`: print 'man' page for the `gsutil`
#'     command or subcommand. Note that only options documented on the
#'     R help page are supported.
#'
#' @param cmd `character()` (optional) command name, e.g.,
#'     `"ls"` for help.
#'
#' @return `gsutil_help()`: the help text, invisbly.
gsutil_help <-
    function(cmd = character(0))
{
    stopifnot(.is_character_0_or_1(cmd))
    result <- .gsutil_do(c("help", cmd))
    cat(noquote(result), sep="\n")
    invisible(result)
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
