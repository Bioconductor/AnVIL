#' @rdname avworkspace
#'
#' @name avworkspace
#'
#' @title Workspace management
NULL

#' @rdname avworkspace
#'
#' @description `avworkspace_clone()` clones (copies) an existing
#'     workspace, possibly into a new namespace (billing account).
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
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
