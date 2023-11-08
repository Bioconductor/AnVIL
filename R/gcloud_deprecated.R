##
## gcloud_sdk_result constructor and methods
##
.gcloud_sdk_result <-
    function(x)
{
    if (!is.null(x))
        class(x) <- "gcloud_sdk_result"
    x
}

#' @export
print.gcloud_sdk_result <-
    function(x, ...)
{
    if (is.null(x))
        return()
    cat(noquote(x), sep="\n")
}


## option or environment variable or NULL, allowing for default
## `unset` value
.gcloud_sdk_getenv <-
    function(option, unset = NA)
{
    value <- getOption(option, unset)
    if (!is.na(value))
        return(value)

    value <- Sys.getenv(option, unset = unset)
    if (!is.na(value))
        return(value)

    return(NULL)
}

## Get gsutil binary on user's machine for windows/linux/mac
.gcloud_sdk_find_binary <-
    function(binary_name)
{
    user_path <- .gcloud_sdk_getenv("GCLOUD_SDK_PATH")
    if (!is.null(user_path))
        return(normalizePath(file.path(user_path, "bin", binary_name)))

    bin <- Sys.which(binary_name)
    if (nzchar(bin))
        return(normalizePath(bin))

    ## Discover binary automatically if user doesn't give path
    if (.Platform$OS.type == "windows") {
        appdata <- normalizePath(Sys.getenv("localappdata"), winslash = "/")
        sdk_path <- file.path("Google", "Cloud SDK", "google-cloud-sdk", "bin")
        binary_cmd <- paste(binary_name, "cmd", sep = ".")
        bin_paths <- c(
            file.path(appdata, sdk_path, binary_cmd),
            file.path(Sys.getenv("ProgramFiles"), sdk_path, binary_cmd),
            file.path(Sys.getenv("ProgramFiles(x86)"), sdk_path, binary_cmd)
        )
    } else {
        bin_paths <- file.path("~", "google-cloud-sdk", "bin", binary_name)
    }

    ## Return appropriate path for 'gsutil'
    for (path in bin_paths)
        if (file.exists(path))
            return(normalizePath(path))

    stop(
        "failed to find '", binary_name, "' binary; ",
        "set option or environment variable 'GCLOUD_SDK_PATH'?",
        call. = FALSE
    )
}

.gcloud_sdk_do <-
    function(command, args)
{
    stopifnot(
        .is_scalar_character(command),
        .is_character(args, na.ok = FALSE)
    )
    bin <- .gcloud_sdk_find_binary(command)
    stopifnot(file.exists(bin))

    value <- withCallingHandlers({
        tryCatch({
            system2(bin, args, stdout = TRUE, stderr = TRUE, wait=TRUE)
        }, error = function(err) {
            msg <- paste0(
                "'", command, " ", paste(args, collapse = " "), "' failed:\n",
                "  ", conditionMessage(err)
            )
            stop(msg, call. = FALSE)
        })
    }, warning = function(warn) {
        invokeRestart("muffleWarning")
    })

    if (!is.null(attr(value, "status"))) {
        msg <- paste0(
            "'", command, " ", paste(args, collapse = " "), "' failed:",
            "\n  ", paste(as.vector(value), collapse = "\n    "),
            "\n  exit status: ", attr(value, "status")
        )
        stop(msg, call. = FALSE)
    }

    value
}

.gcloud_do <-
    function(...)
{
    .gcloud_sdk_do("gcloud", c(...))
}

.gcloud_access_token_new <-
    function(app_default, now)
{
    ## obtain the access token
    token <- .gcloud_do("auth", app_default, "print-access-token")

    ## There is only one token per service account, so requesting a
    ## token may return one with expiration less than 60 minutes. So
    ## check the actual expiry time of the token.
    ##
    ## Calculating expiry from before the call (`now` argument) is
    ## conservative -- we underestimate the time by the latency
    ## involved in the POST and result parsing.
    response <- POST(
        "https://www.googleapis.com/oauth2/v1/tokeninfo",
        httr::content_type("application/x-www-form-urlencoded"),
        body = paste0("access_token=", token)
    )
    .avstop_for_status(response, ".gcloud_access_token_expires")
    expires <- now + content(response)$expires_in

    list(token = token, expires = expires)
}

.gcloud_access_token <-
    local(
{
    tokens <- new.env(parent = emptyenv())
    function(service) {
        app_default <-
            if (identical(Sys.getenv("USER"), "jupyter-user"))
                "application-default"

        key <- paste0(service, ":", app_default, ":", gcloud_account())
        now <- Sys.time()
        if (is.null(tokens[[key]])) {
            tokens[[key]] <- .gcloud_access_token_new(app_default, now)
        } else {
            expires_in <- tokens[[key]]$expires - now
            if (expires_in < 1L) {
                ## allow a nearly expired token to fully expire
                if (expires_in > 0L)
                    Sys.sleep(expires_in)
                tokens[[key]] <- .gcloud_access_token_new(app_default, now)
            }
        }

        tokens[[key]]$token
    }
})

#' @rdname AnVIL-deprecated
#'
#' @name gcloud-gsutil
#'
#' @title gcloud and gsutil command line utility interfaces (DEPRECATED)
#'
#' @description These functions invoke the `gcloud` and `gsutil` command line
#'     utilities. See \link{gsutil} for details on how `gcloud` is
#'     located.
NULL

#' @rdname AnVIL-deprecated
#'
#' @description `gcloud_exists()` tests whether the `gcloud()` command
#'     can be found on this system. See 'Details' section of `gsutil`
#'     for where the application is searched.
#'
#' @return `gcloud_exists()` returns `TRUE` when the `gcloud`
#'     application can be found, FALSE otherwise.
#'
#' @examples
#' gcloud_exists()
#'
#' @export
gcloud_exists <-
    function()
{
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    result <- tryCatch({
        .gcloud_sdk_find_binary("gcloud")
    }, error = function(...) "")
    nchar(result) > 0L
}

#' @importFrom utils tail
.gcloud_get_value_check <-
    function(result, function_name)
{
    value <- tail(result, 1L)
    if (identical(value, "(unset)")) {
        message <- paste0(
            "'", function_name, "()' returned '(unset)'; this may indicate ",
            "that the gcloud active configuration is incorrect. Try ",
            "`gcloud auth application-default login` at the command line"
        )
        warning(paste(strwrap(message), collapse = "\n"))
    }
    value
}

#' @rdname AnVIL-deprecated
#'
#' @description `gcloud_account()`: report the current gcloud account
#'     via `gcloud config get-value account`.
#'
#' @param account character(1) Google account (e.g., `user@gmail.com`)
#'     to use for authentication.
#'
#' @return `gcloud_account()` returns a `character(1)` vector
#'     containing the active gcloud account, typically a gmail email
#'     address.
#'
#' @examples
#' if (gcloud_exists())
#'     gcloud_account()
#'
#' @export
gcloud_account <- function(account = NULL) {
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    stopifnot(is.null(account) || .is_scalar_character(account))

    if (!is.null(account))
        .gcloud_do("config", "set", "account", account)
    result <- .gcloud_do("config", "get-value", "account")
    .gcloud_get_value_check(result, "gcloud_account")
}

#' @rdname AnVIL-deprecated
#'
#' @description `gcloud_project()`: report the current gcloud project
#'     via `gcloud config get-value project`.
#'
#' @param project character(1) billing project name.
#'
#' @return `gcloud_project()` returns a `character(1)` vector
#'     containing the active gcloud project.
#'
#' @export
gcloud_project <- function(project = NULL) {
    stopifnot(
        is.null(project) || .is_scalar_character(project)
    )
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    if (!is.null(project))
        .gcloud_do("config", "set", "project", project)
    result <- .gcloud_do("config", "get-value", "project")
    ## returns two lines when `CLOUDSDK_ACTIVE_CONFIG_NAME=`
    ## envirionment variable is set
    .gcloud_get_value_check(result, "gcloud_account")
}

#' @rdname AnVIL-deprecated
#'
#' @description `gcloud_help()`: queries `gcloud` for help for a
#'     command or sub-comand via `gcloud help ...`.
#'
#' @param ... Additional arguments appended to gcloud commands.
#'
#' @return `gcloud_help()` returns an unquoted `character()` vector
#'     representing the text of the help manual page returned by
#'     `gcloud help ...`.
#'
#' @examples
#' if (gcloud_exists())
#'     gcloud_help()
#'
#' @export
gcloud_help <- function(...) {
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    .gcloud_sdk_result(.gcloud_do("help", ...))
}

#' @rdname AnVIL-deprecated
#'
#' @description `gcloud_cmd()` allows arbitrary `gcloud` command
#'     execution via `gcloud ...`. Use pre-defined functions in
#'     preference to this.
#'
#' @param cmd `character(1)` representing a command used to evaluate
#'     `gcloud cmd ...`.
#'
#' @return `gcloud_cmd()` returns a `character()` vector representing
#'     the text of the output of `gcloud cmd ...`
#'
#' @export
gcloud_cmd <- function(cmd, ...) {
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    .gcloud_do(cmd, ...)
}


#' @rdname AnVIL-deprecated
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

.gsutil_sh_quote <-
    function(source)
{
    ## Expand local paths with ~ or . or ... to full path names.
    ## Needed because we also use shQuote() (to allow for spaces in
    ## file names), and shQuote() would otherwise use paths with ~ or
    ## . in the current working directory.
    is_local <- !.gsutil_is_uri(source)
    source[is_local] <- normalizePath(source[is_local])
    shQuote(source)
}

#' @rdname AnVIL-deprecated
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
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    project <- gcloud_project()
    buckets <- regmatches(source, regexpr("^gs://[^/]+", source))
    is_enabled <- FALSE
    for (bucket in buckets) {
        args <- c("-u", project, "requesterpays", "get", bucket)
        result <- .gsutil_do(args)
        is_enabled <- endsWith(result, "Enabled")
        if (is_enabled)
            break
    }
    is_enabled
}

.gsutil_requesterpays_flag <-
    function(source)
{
    source <- source[.gsutil_is_uri(source)]
    tryCatch({
        if (length(source) && gsutil_requesterpays(source)) {
            c("-u", gcloud_project())
        } else NULL
    }, error = function(e) {
        ## this was originally written to return NULL without a
        ## warning, but I'm not sure whether we cannot just stop()?
        warning(
            "'gsutil_requesterpays()' returned an error:",
            "\n  ", conditionMessage(e),
            call. = FALSE
        )
        NULL
    })
}

#' @rdname AnVIL-deprecated
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
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    args <- c(
        .gsutil_requesterpays_flag(source),
        "ls",
        if (recursive) "-r",
        ...,
        shQuote(source)
    )
    result <- .gsutil_do(args)
    result[nzchar(result) & !endsWith(result, ":")]
}

.gsutil_exists_1 <-
    function(source, gsutil)
{
    args <- c(
        .gsutil_requesterpays_flag(source),
        "ls",
        shQuote(source)
    )
    value <- withCallingHandlers({
        system2(gsutil, args, stdout = TRUE, stderr = TRUE, wait=TRUE)
    }, warning = function(w) {
        invokeRestart("muffleWarning")
    })
    is.null(attr(value, "status"))
}

#' @rdname AnVIL-deprecated
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
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    gsutil <- .gcloud_sdk_find_binary("gsutil")
    stopifnot(file.exists(gsutil))      # bad environment variables

    source <- setNames(source, source)
    vapply(source, .gsutil_exists_1, logical(1), gsutil)
}

#' @rdname AnVIL-deprecated
#'
#' @description `gsutil_stat()`: print, as a side effect, the status
#'     of a bucket, directory, or file.
#'
#' @return `gsutil_stat()`: `tibble()` summarizing status of each
#'     bucket member.
#'
#' @examples
#' if (gcloud_exists()) {
#'     gsutil_exists(src)
#'     gsutil_stat(src)
#'     gsutil_ls(dirname(src))
#' }
#'
#' @importFrom tidyr pivot_wider
#' @export
gsutil_stat <-
    function(source)
{
    stopifnot(.gsutil_is_uri(source))
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    args <- c(.gsutil_requesterpays_flag(source), "stat", shQuote(source))
    result <- .gsutil_do(args)

    ## omit nested 'metadata', for convenience
    is_metadata <- grepl("^( {4}Metadata:| {8})", result)
    result <- result[!is_metadata]

    ## form into tibble with rows for each bucket & key / value pair
    is_path <- startsWith(result, "gs://")
    group <- cumsum(is_path)
    n <- tabulate(group) - 1L
    re <- "^ +([^:]+): +(.*)"
    tz_format <- "%a, %d %b %Y %H:%M:%S"
    tbl <- tibble(
        path = rep(sub(":$", "", result[is_path]), n),
        key = sub(re, "\\1", result[!is_path]),
        value = sub(re, "\\2", result[!is_path])
    )

    ## reshape to one row per bucket
    tbl %>%
        pivot_wider(
            id_cols = .data$path, names_from = "key", values_from = "value"
        ) %>%
        mutate(
            `Creation time` =
                as.POSIXct(.data$`Creation time`, tz = "GMT", format = tz_format),
            `Update time` =
                as.POSIXct(.data$`Update time`, tz = "GMT", format = tz_format)
        )
}

#' @rdname AnVIL-deprecated
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
#' if (gcloud_exists()) {
#'    gsutil_cp(src, tempdir())
#'    ## gsutil_*() commands work with spaces in the source or destination
#'    destination <- file.path(tempdir(), "foo bar")
#'    gsutil_cp(src, destination)
#'    file.exists(destination)
#' }
#'
#' @export
gsutil_cp <-
    function(source, destination, ..., recursive = FALSE, parallel = TRUE)
{
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    location <- c(source, destination)
    location_is_uri <- .gsutil_is_uri(location)
    stopifnot(
        .is_character(source), .is_scalar_character(destination),
        any(location_is_uri),
        .is_scalar_logical(recursive), .is_scalar_logical(parallel)
    )

    args <- c(
        .gsutil_requesterpays_flag(location),
        if (parallel) "-m", ## Makes the operations faster
        "cp", ## cp command
        if (recursive) "-r",
        ...,
        .gsutil_sh_quote(source),
        .gsutil_sh_quote(destination)
    )
    result <- .gsutil_do(args)
    .gcloud_sdk_result(result)
}

#' @rdname AnVIL-deprecated
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
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    ## remove
    args <- c(
        .gsutil_requesterpays_flag(source),
        if (parallel) "-m",
        "rm",
        if (force) "-f",
        if (recursive) "-r",
        ...,
        shQuote(source)
    )
    result <- .gsutil_do(args)
    .gcloud_sdk_result(result)
}

#' @rdname AnVIL-deprecated
#'
#' @description `gsutil_rsync()`: synchronize a source and a
#'     destination. If the destination is on the local file system, it
#'     must be a directory or not yet exist (in which case a directory
#'     will be created).
#'
#' @param exclude `character(1)` a python regular expression of bucket
#'     paths to exclude from synchronization. E.g.,
#'     `'.*(\\.png|\\.txt)$"` excludes '.png' and .txt' files.
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
    function(source, destination, ..., exclude = NULL, dry = TRUE,
        delete = FALSE, recursive = FALSE, parallel = TRUE)
{
    stopifnot(
        .is_scalar_character(source), .is_scalar_character(destination),
        .gsutil_is_uri(source) || .gsutil_is_uri(destination),
        is.null(exclude) || .is_scalar_character(exclude),
        .is_scalar_logical(dry),
        .is_scalar_logical(delete),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel)
    )
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
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
        if (length(exclude)) paste0('-x "', exclude, '"'),
        if (dry) "-n",
        if (delete) "-d",
        if (recursive) "-r",
        ...,
        .gsutil_sh_quote(source),
        .gsutil_sh_quote(destination)
    )
    result <- .gsutil_do(args)
    .gcloud_sdk_result(result)
}

#' @rdname AnVIL-deprecated
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
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
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
        shQuote(source)
    )

    .gsutil_do(args)
}

#' @rdname AnVIL-deprecated
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
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    result <- .gsutil_do(c("help", cmd))
    .gcloud_sdk_result(result)
}

##
## higher-level implementations
##

#' @rdname AnVIL-deprecated
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
    .Deprecated(
        msg = c(
            "'gcloud_*' and 'gsutil_*' functions are deprecated.\n",
            "Use the 'AnVILGCP' package instead.\n",
            "See help(\"AnVIL-deprecated\")"
        )
    )
    is_read <- identical(substr(open, 1, 1), "r")
    args <- c(
        if (is_read) .gsutil_requesterpays_flag(source),
        "cp",
        ...,
        if (is_read) c(shQuote(source), "-") else c("-", shQuote(source))
    )

    bin <- .gcloud_sdk_find_binary("gsutil")
    stopifnot(file.exists(bin))

    cmd <- paste(c(bin, args), collapse = " ")
    pipe(cmd, open)
}
