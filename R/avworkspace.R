#' @rdname avworkspace
#'
#' @name avworkspace
#'
#' @title Workspace management
NULL

##
## utilities
##

.avworkspace <- local({
    hash <- new.env(parent = emptyenv())
    function(fun, key, value) {
        sysvar <- toupper(paste0("WORKSPACE_", key))
        if (is.null(value)) {
            if (is.null(hash[[key]])) {
                ## initialize
                hash[[key]] <- Sys.getenv(sysvar)
                if (!nzchar(hash[[key]]) && interactive())
                    warning("'", sysvar, "' undefined; use `", fun, "()` to set")
            }
        } else {
            hash[[key]] <- ifelse(is.na(value), Sys.getenv(sysvar), value)
        }
        hash[[key]]
    }
})
#' @rdname avworkspace
#'
#' @description `avworkspace_namespace()` and `avworkspace_name()` are
#'     utiliity functions to retrieve workspace namespace and name
#'     from environment variables or interfaces usually available in
#'     AnVIL notebooks or RStudio sessions.  `avworkspace()` provides
#'     a convenient way to specify workspace namespace and name in a
#'     single command.
#'
#' @details `avworkspace_namespace()` is the billing account. If the
#'     `namespace=` argument is not provided, try `gcloud_project()`,
#'     and if that fails try `Sys.getenv("WORKSPACE_NAMESPACE")`.
#'
#'     `avworkspace_name()` is the name of the workspace as it appears
#'     in \url{https://app.terra.bio/#workspaces}. If not provided,
#'     `avworkspace_name()` tries to use
#'     `Sys.getenv("WORKSPACE_NAME")`.
#'
#'     Namespace and name values are cached across sessions, so
#'     explicitly providing `avworkspace_name*()` is required at most
#'     once per session. Revert to system settings with arguments
#'     `NA`.
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @param workspace when present, a `character(1)` providing the
#'     concatenated namespace and name, e.g.,
#'     `"bioconductor-rpci-anvil/Bioconductor-Package-AnVIL"`
#'
#' @return `avworkspace_namespace()`, and `avworkspace_name()` return
#'     `character(1)` identifiers. `avworkspace()` returns the
#'     character(1) concatenated namespace and name. The value
#'     returned by `avworkspace_name()` will be percent-encoded (e.g.,
#'     spaces `" "` replaced by `"%20"`).
#'
#' @examples
#' avworkspace_namespace()
#' avworkspace_name()
#' avworkspace()
#'
#' @export
avworkspace_namespace <- function(namespace = NULL) {
    suppressWarnings({
        namespace <- .avworkspace("avworkspace_namespace", "NAMESPACE", namespace)
    })
    if (!nzchar(namespace)) {
        namespace <- tryCatch({
            gcloud_project()
        }, error = function(e) {
            NULL
        })
        namespace <- .avworkspace("avworkspace_namespace", "NAMESPACE", namespace)
    }
    namespace
}

#' @rdname avworkspace
#'
#' @importFrom utils URLencode
#'
#' @export
avworkspace_name <-
    function(name = NULL)
{
    value <- .avworkspace("avworkspace_name", "NAME", name)
    URLencode(value)
}

#' @rdname avworkspace
#'
#' @export
avworkspace <-
    function(workspace = NULL)
{
    stopifnot(
        is.null(workspace) || .is_scalar_character(workspace)
    )
    if (!is.null(workspace)) {
        wkspc <- strsplit(workspace, "/")[[1]]
        if (length(wkspc) != 2L)
            stop(
                "'workspace' must be of the form 'namespace/name', ",
                "with a single '/'"
            )
        avworkspace_namespace(wkspc[[1]])
        avworkspace_name(wkspc[[2]])
    }
    paste0(avworkspace_namespace(), "/", avworkspace_name())
}

#' @rdname avworkspace
#'
#' @description `avworkspace_clone()` clones (copies) an existing
#'     workspace, possibly into a new namespace (billing account).
#'
#' @param to_namespace character(1) workspace (billing account) in
#'     which to make the clone.
#'
#' @param to_name character(1) name of the cloned workspace.
#'
#' @return `avworkspace_clone()` returns the namespace and name, in
#'     the format `namespace/name`, of the cloned workspace.
#'
#' @export
avworkspace_clone <-
    function(
             namespace = avworkspace_namespace(),
             name = avworkspace_name(),
             to_namespace = namespace,
             to_name)
{
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_character(to_namespace),
        .is_scalar_character(to_name),
        `source and destination 'namespace/name' must be different` =
            !identical(namespace, to_namespace) || !identical(name, to_name)
    )

    response <- Terra()$cloneWorkspace(
        workspaceNamespace = namespace,
        workspaceName = name,
        .__body__ = list(
            attributes = setNames(list(), character()),  # json '{}'
            copyFilesWithPrefix = "notebooks/",
            namespace = to_namespace,
            name = to_name
        )
        )
    .avstop_for_status(response, "avworkspace_clone")

    paste(to_name, to_namespace, sep = "/")
}
