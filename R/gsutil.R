#' @rdname gsutil
#'
#' @name gsutil
#'
#' @title gsutil command line utility interface
#'
#' @description These functions invoke the `gsutil` command line
#'     utility. See the "Details:" section if you have gsutil
#'     installed but the package cannot find it.
#'
#' @details The `gsutil` system command is required.  The search for
#'     `gsutil` starts with environment variable `GCLOUD_SDK_PATH`
#'     providing a path to a directory containing a `bin` directory
#'     containingin `gsutil`, `gcloud`, etc. The path variable is
#'     searched for first as an `option()` and then system
#'     variable. If no option or global variable is found,
#'     `Sys.which()` is tried. If that fails, `gsutil` is searched for
#'     on defined paths. On Windows, the search tries to find
#'     `Google\\Cloud SDK\\google-cloud-sdk\\bin\\gsutil.cmd` in the
#'     `LOCAL APP DATA`, `Program Files`, and `Program Files (x86)`
#'     directories.  On linux / macOS, the search continues with
#'     `~/google-cloud-sdk`.
#'
#' @examples
#'     src <- "gs://genomics-public-data/1000-genomes/other/sample_info/sample_info.csv"
NULL

## evaluate the gsutil command and arguments in `args`
.gsutil_do <-
    function(args)
{
    .gcloud_sdk_do("gsutil", args)
}

.gsutil_is_uri <-
    function(source)
{
    .is_character(source) & grepl("gs://[^/]+", source)
}

#' @rdname gsutil
#'
#' @description `gsutil_requesterpays()`: does the google bucket
#'     require that the requester pay for access?
#'
#' @return `gsutil_requesterpays()`: named `logical()` vector TRUE
#'     when requester-pays is enabled.
#'
#' @examples
#' if (gcloud_exists())
#'     gsutil_requesterpays(src) # FALSE -- no cost download
#'
#' @export
gsutil_requesterpays <-
    function(source)
{
    stopifnot(all(.gsutil_is_uri(source)))
    project <- gcloud_project()
    buckets <- regmatches(source, regexpr("^gs://[^/]+", source))
    args <- c("-u", project, "requesterpays", "get", buckets)
    result <- .gsutil_do(args)
    setNames(endsWith(result, "Enabled"), source)
}

.gsutil_requesterpays_flag <-
    function(source)
{
    tryCatch({
        if (any(gsutil_requesterpays(source))) {
            c("-u", gcloud_project())
        } else NULL
    }, error = function(...) {
        NULL
    })
}

#' @rdname gsutil
#'
#' @description `gsutil_ls()`: List contents of a google cloud bucket
#'     or, if `source` is missing, all Cloud Storage buckets under
#'     your default project ID
#'
#' @param source `character(1)`, (`character()` for
#'     `gsutil_requesterpays()`, `gsutil_ls()`, `gsutil_exists()`,
#'     `gsutil_cp()`) paths to a google storage bucket, possibly with
#'     wild-cards for file-level pattern matching.
#'
#' @param recursive `logical(1)`; perform operation recursively from
#'     `source`?. Default: `FALSE`.
#'
#' @param ... additional arguments passed as-is to the `gsutil` subcommand.
#'
#' @return `gsutil_ls()`: `character()` listing of `source` content.
#'
#' @export
gsutil_ls <-
    function(source = character(), ..., recursive = FALSE)
{
    stopifnot(
        .gsutil_is_uri(source),
        .is_scalar_logical(recursive)
    )

    args <- c(
        .gsutil_requesterpays_flag(source),
        "ls",
        if (recursive) "-r",
        ...,
        source
    )
    result <- .gsutil_do(args)
    result[nzchar(result) & !endsWith(result, ":")]
}

.gsutil_exists_1 <-
    function(source, gsutil)
{
    args <- c("ls", source)
    value <- withCallingHandlers({
        system2(gsutil, args, stdout = TRUE, stderr = TRUE, wait=TRUE)
    }, warning = function(w) {
        invokeRestart("muffleWarning")
    })
    is.null(attr(value, "status"))
}

#' @rdname gsutil
#'
#' @description `gsutil_exists()`: check if the bucket or object
#'     exists.
#'
#' @return `gsutil_exists()`: logical(1) TRUE if bucket or object exists.
#'
#' @export
gsutil_exists <-
    function(source)
{
    stopifnot(
        is.character(source), !anyNA(source),
        .gsutil_is_uri(source)
    )

    gsutil <- .gcloud_sdk_find_binary("gsutil")
    stopifnot(file.exists(gsutil))      # bad environment variables

    vapply(source, .gsutil_exists_1, logical(1), gsutil)
}

#' @rdname gsutil
#'
#' @description `gsutil_stat()`: print, as a side effect, the status
#'     of a bucket, directory, or file.
#'
#' @return `gsutil_stat()`: `character()` description of status of
#'     objects matching `source`.
#' @examples
#' if (gcloud_exists()) {
#'     gsutil_exists(src)
#'     gsutil_stat(src)
#'     gsutil_ls(dirname(src))
#' }
#'
#' @export
gsutil_stat <-
    function(source)
{
    stopifnot(.gsutil_is_uri(source))

    args <- c(.gsutil_requesterpays_flag(source), "stat", source)
    result <- .gsutil_do(args)
    .gcloud_sdk_result(result)
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
#' if (gcloud_exists())
#'    gsutil_cp(src, tempdir())
#'
#' @export
gsutil_cp <-
    function(source, destination, ..., recursive = FALSE, parallel = TRUE)
{
    source_is_uri <- .gsutil_is_uri(source)
    stopifnot(
        .is_character(source), .is_scalar_character(destination),
        all(source_is_uri) || .gsutil_is_uri(destination),
        .is_scalar_logical(recursive), .is_scalar_logical(parallel)
    )

    args <- c(
        if (any(source_is_uri)) .gsutil_requesterpays_flag(source),
        if (parallel) "-m", ## Makes the operations faster
        "cp", ## cp command
        if (recursive) "-r",
        ...,
        source,
        destination
    )
    result <- .gsutil_do(args)
    .gcloud_sdk_result(result)
}

#' @rdname gsutil
#'
#' @description `gsutil_rm()`: remove contents of a google cloud
#'     bucket.
#'
#' @param force `logical(1)`: continue silently despite errors when
#'     removing multiple objects. Default: `FALSE`.
#'
#' @return `gsutil_rm()`: exit status of `gsutil_rm()`, invisibly.
#'
#' @export
gsutil_rm <-
    function(source, ..., force = FALSE, recursive = FALSE, parallel = TRUE)
{
    stopifnot(
        .gsutil_is_uri(source),
        .is_scalar_logical(force),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel)
    )

    ## remove
    args <- c(
        .gsutil_requesterpays_flag(source),
        if (parallel) "-m",
        "rm",
        if (force) "-f",
        if (recursive) "-r",
        ...,
        source
    )
    result <- .gsutil_do(args)
    .gcloud_sdk_result(result)
}

#' @rdname gsutil
#'
#' @description `gsutil_rsync()`: synchronize a source and a destination.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
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
#' @export
gsutil_rsync <-
    function(source, destination, ..., dry = TRUE,
        delete = FALSE, recursive = FALSE, parallel = TRUE)
{
    stopifnot(
        .is_scalar_character(source), .is_scalar_character(destination),
        .gsutil_is_uri(source) || .gsutil_is_uri(destination),
        .is_scalar_logical(dry),
        .is_scalar_logical(delete),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel)
    )
    ## if destination is not a google cloud repo, and does not exist
    if (!dry && !.gsutil_is_uri(destination) && !dir.exists(destination))
        if (!dir.create(destination))
            stop("'gsutil_rsync()' failed to create '", destination, "'")

    ## rsync operation
    args <- c(
        .gsutil_requesterpays_flag(source),
        ##  -m option, to perform parallel (multi-threaded/multi-processing)
        if (parallel) "-m",
        "rsync",
        if (dry) "-n",
        if (delete) "-d",
        if (recursive) "-r",
        ...,
        source,
        destination
    )
    result <- .gsutil_do(args)
    .gcloud_sdk_result(result)
}

#' @rdname gsutil
#'
#' @description `gsutil_cat()`: concatenate bucket objects to standard output
#'
#' @param header `logical(1)` when `TRUE` annotate each
#'
#' @param range (optional) `integer(2)` vector used to form a range
#'     from-to of bytes to concatenate. `NA` values signify
#'     concatenation from the start (first position) or to the end
#'     (second position) of the file.
#'
#' @return `gsutil_cat()` returns the content as a character vector.
#'
#' @export
gsutil_cat <-
    function(source, ..., header = FALSE, range = integer())
{
    stopifnot(
        .is_scalar_character(source),
        .is_scalar_logical(header),
        is.numeric(range),
        all(range[!is.na(range)] >= 0),
        all(diff(range[!is.na(range)]) > 0L),
        length(range) == 0L || length(range) == 2L
    )

    if (length(range)) {
        range[is.na(range)] <- ""
        range <- paste(range, collapse="-")
    }

    args <- c(
        .gsutil_requesterpays_flag(source),
        "cat",
        if (header) "-h",
        if (length(range)) c("-r", range),
        source
    )

    .gsutil_do(args)
}

#' @rdname gsutil
#'
#' @description `gsutil_help()`: print 'man' page for the `gsutil`
#'     command or subcommand. Note that only commandes documented on this
#'     R help page are supported.
#'
#' @param cmd `character()` (optional) command name, e.g.,
#'     `"ls"` for help.
#'
#' @return `gsutil_help()`: `character()` help text for subcommand `cmd`.
#'
#' @examples
#' if (gcloud_exists())
#'     gsutil_help("ls")
#'
#' @export
gsutil_help <-
    function(cmd = character(0))
{
    stopifnot(.is_character_0_or_1(cmd))
    result <- .gsutil_do(c("help", cmd))
    .gcloud_sdk_result(result)
}

##
## higher-level implementations
##

#' @rdname gsutil
#'
#' @description `gsutil_pipe()`: create a pipe to read from or write
#'     to a gooogle bucket object.
#'
#' @param open `character(1)` either `"r"` (read) or `"w"` (write)
#'     from the bucket.
#'
#' @return `gsutil_pipe()` an unopened R `pipe()`; the mode is
#'     \emph{not} specified, and the pipe must be used in the
#'     appropriate context (e.g., a pipe created with `open = "r"` for
#'     input as `read.csv()`)
#'
#' @examples
#' if (gcloud_exists()) {
#'     df <- read.csv(gsutil_pipe(src), 5L)
#'     class(df)
#'     dim(df)
#'     head(df)
#' }
#'
#' @export
gsutil_pipe <-
    function(source, open = "r", ...)
{
    stopifnot(
        .is_scalar_character(source),
        .is_scalar_character(open)
    )

    is_read <- identical(substr(open, 1, 1), "r")
    args <- c(
        if (is_read) .gsutil_requesterpays_flag(source),
        "cp",
        ...,
        if (is_read) c(source, "-") else c("-", source)
    )

    bin <- .gcloud_sdk_find_binary("gsutil")
    stopifnot(file.exists(bin))

    cmd <- paste(c(bin, args), collapse = " ")
    pipe(cmd, open)
}
