#' @rdname avworkspace
#'
#' @name avworkspace
#'
#' @title Workspace management
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
#' @param bucket_location character(1) region (NO multi-region, except
#'     the default) in which bucket attached to the workspace should
#'     be created.
#'
#' @return `avworkspace_clone()` returns the namespace and name, in
#'     the format `namespace/name`, of the cloned workspace.
#'
#' @importFrom utils URLencode
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
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_character(to_namespace),
        .is_scalar_character(to_name),
        .is_scalar_character(bucket_location),
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
