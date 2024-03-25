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
        isScalarCharacter(command),
        isCharacter(args, na.ok = FALSE)
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
