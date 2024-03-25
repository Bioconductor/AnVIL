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
    function(fun, key, value, warn = TRUE) {
        sysvar <- toupper(paste0("WORKSPACE_", key))
        if (is.null(value)) {
            if (is.null(hash[[key]])) {
                ## initialize
                hash[[key]] <- Sys.getenv(sysvar)
                if (!nzchar(hash[[key]]) && warn && interactive())
                    warning("'", sysvar, "' undefined; use `", fun, "()` to set")
            }
        } else {
            hash[[key]] <- ifelse(is.na(value), Sys.getenv(sysvar), value)
        }
        hash[[key]]
    }
})

.avworkspaces_clean <- function(.data) {
    .data |>
        select(
            name = .data$workspace.name,
            lastModified = .data$workspace.lastModified,
            createdBy = .data$workspace.createdBy,
            namespace = .data$workspace.namespace,
            accessLevel = .data$accessLevel
        ) |>
        mutate(
            lastModified = as.Date(.data$lastModified)
        ) |>
        arrange(
            .data$name,
            desc(.data$lastModified)
        )
}

#' @rdname avworkspace
#'
#' @description `avworkspaces()` returns a tibble with available
#'     workspaces.
#'
#' @return `avworkspaces()` returns a tibble with columns including
#'     the name, last modification time, namespace, and owner status.
#'
#'
#' @export
avworkspaces <-
    function()
{

    response <- Terra()$listWorkspaces()
    .avstop_for_status(response, "avworkspaces")

    flatten(response) %>%
        .avworkspaces_clean()
}

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
#' `avworkspace_name()` is the name of the workspace as it appears in
#' \url{https://app.terra.bio/#workspaces}. If not provided,
#' `avworkspace_name()` tries to use `Sys.getenv("WORKSPACE_NAME")`.
#'
#' Namespace and name values are cached across sessions, so explicitly
#' providing `avworkspace_name*()` is required at most once per
#' session. Revert to system settings with arguments `NA`.
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @param warn logical(1) when `TRUE` (default), generate a warning
#'     when the workspace namespace or name cannot be determined.
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
avworkspace_namespace <-
    function(namespace = NULL, warn = TRUE)
{
    namespace <- .avworkspace(
        "avworkspace_namespace", "NAMESPACE", namespace, warn = FALSE
    )
    if (!nzchar(namespace)) {
        namespace <- tryCatch({
            gcloud_project()
        }, error = function(e) {
            NULL
        })
        namespace <- .avworkspace(
            "avworkspace_namespace", "NAMESPACE", namespace, warn = warn
        )
    }
    namespace
}

#' @rdname avworkspace
#'
#' @importFrom utils URLencode
#'
#' @export
avworkspace_name <-
    function(name = NULL, warn = TRUE)
{
    value <- .avworkspace("avworkspace_name", "NAME", name, warn = warn)
    URLencode(value)
}

#' @rdname avworkspace
#'
#' @export
avworkspace <-
    function(workspace = NULL)
{
    stopifnot(
        `'workspace' must be NULL or of the form 'namespace/name'` =
            is.null(workspace) || .is_workspace(workspace)
    )
    if (!is.null(workspace)) {
        wkspc <- strsplit(workspace, "/")[[1]]
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
#' @param bucket_location character(1) region (NO multi-region, except
#'     the default) in which bucket attached to the workspace should
#'     be created.
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
             to_name,
             bucket_location = "US")
{
    stopifnot(
        isScalarCharacter(namespace),
        isScalarCharacter(name),
        isScalarCharacter(to_namespace),
        isScalarCharacter(to_name),
        isScalarCharacter(bucket_location),
        `source and destination 'namespace/name' must be different` =
            !identical(namespace, to_namespace) || !identical(name, to_name)
    )

    response <- Terra()$cloneWorkspace(
        workspaceNamespace = namespace,
        workspaceName = URLencode(name),
        .__body__ = list(
            attributes = setNames(list(), character()),  # json '{}'
            bucketLocation = bucket_location,
            copyFilesWithPrefix = "notebooks/",
            namespace = to_namespace,
            name = URLencode(to_name)
        )
        )
    .avstop_for_status(response, "avworkspace_clone")

    paste(to_namespace, to_name, sep = "/")
}
