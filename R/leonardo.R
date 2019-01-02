##
## Clusters
##

#' @importFrom curl curl_escape
#' @importFrom httr config accept_json verbose add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble

#' @rdname leonardo
#'
#' @title Interact with Leonardo REST API
#'
#' @description Functions on this page interact with Leonardo's REST
#'     api, documented at `leonardo.dev.anvilproject.org/#/`. See
#'     `?authenticate` for OAuth 2.0 authentication.
#'
#' @details `api_clusters()` lists all active clusters. 
#'
#' @param googleProject (optional) character(1) name of google
#'     project, e.g., `"anvil-leo-dev"`
#'
#' @param ... (optional) for `api_clusters()`, key-value pairs of
#'     'labels' to filter running clusters, e.g., `clusterName =
#'     "jupyter_bioc"`.
#'
#' @param includeDeleted (optional) logical(1) include deleted clusters?
#'
#' @param verbose (optional) logical(1) report http activity; see
#'     `?httr::verbose`.
#'
#' @return `api_clusters()` returns a tibble of running (or running
#'     and deleted) clusters.
#'
#' @examples
#' if (interactive())
#'     api_clusters() %>% select(starts_with("label"))
#' @export
api_clusters <-
    function(googleProject = NULL, ..., includeDeleted = FALSE, verbose = FALSE)
{
    dots <- list(...)
    stopifnot(
        is.null(googleProject) || .is_scalar_character(googleProject),
        all(vapply(dots, .is_scalar_character, logical(1))),
        .is_scalar_logical(includeDeleted),
        .is_scalar_logical(verbose)
    )

    path <- .api_clusters_path(googleProject, dots, includeDeleted)
    token <- authenticate()
    response <- .get(path, config(token = token), verbose)
    df <- fromJSON(response, flatten=TRUE)
    as_tibble(df)
}

.api_clusters_path <-
    function(googleProject, dots, includeDeleted)
{
    if (!is.null(googleProject))
        googleProject <- paste0("/", googleProject)

    labels_ <- NULL
    if (length(dots)) {
        labels_ <- paste0(
            "_labels=",
            curl_escape(paste0(names(dots), "=", unname(dots), collapse=",")),
            "&"
        )
    }

    includeDeleted <- paste0("includeDeleted=", tolower(includeDeleted))

    paste0("/api/clusters", googleProject, "?", labels_, includeDeleted)
}

#' @rdname leonardo
#'
#' @details `api_cluster()` gets details of a single Dataproc cluster.
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#' @return `api_cluster()` returns a list-of-lists describing a single
#'     cluster.
#' 
#' @export
api_cluster <-
    function(googleProject = "anvil-leo-dev", clusterName, verbose = FALSE)
{
    stopifnot(
        .is_scalar_character(googleProject),
        .is_scalar_character(clusterName)
    )

    path <- sprintf("/api/cluster/%s/%s", googleProject, clusterName)
    token <- authenticate()
    response <- .get(path, config(token = token), verbose)
    fromJSON(response)
}


#' @rdname leonardo
#'
#' @details `api_create_cluster()` creates a new dataproc cluster in
#'     the given project with a given cluster name and cluster
#'     request.
#'
#' @param googleProject character(1) name of google project, e.g,
#'     `"anvil-leo-dev"`
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#' @param clusterRequest list(1) request to a cluster resource in the
#'     form of list(requestName = clusterRequest). clusterRequest
#'     needs to be taken from available resources on Google container
#'     registry eg:
#'     "us.gcr.io/anvil-leo-dev/anvil_bioc_docker:latest".
#'
#' @return `api_create_cluster()` returns a json list-of-lists
#'     describing the cluster which was created.
#'
#' @export
api_create_cluster <-
    function(googleProject, clusterName, clusterRequest,
             verbose = FALSE)
{
    stopifnot(
        .is_scalar_character(googleProject),
        .is_scalar_character(clusterName),
        is.list(clusterRequest)
    )

    path <- sprintf("/api/cluster/v2/%s/%s", googleProject, clusterName)
    token  <- authenticate()
    response <- .put(path, config(token = token), body = clusterRequest, verbose)
    fromJSON(response)
}



##
## notebooks
##

#' @rdname leonardo
#'
#' @details `notebooks_cluster_name()` proxies all requests through
#'     the jupyter notebook server running on a cluster
#'
#' @param googleProject character(1) name of google project, e.g,
#'     `"anvil-leo-dev"`
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#' @return
#'
#' @export
notebooks_cluster_name <-
    function(googleProject, clusterName)
{
    path <- sprintf("/notebooks/%s/%s", googleProject, clusterName)
    token <- authenticate()
    response <- .get(path, config(token=token), verbose)
    fromJSON(response)
}



#' @rdname leonardo
#'
#' @details `notebooks_set_cookie()`facilitates setting a cookie
#'     containing the Google token.
#'
#' @param googleProject character(1) name of google project, e.g,
#'     `"anvil-leo-dev"`
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#' @return
#'
#' @export
notebooks_set_cookie <-
    function(googleProject, clusterName)
{

    path <- sprintf("/notebooks/%s/%s/setCookie", googleProject,
                    clusterName)
    token <- authenticate()
    response <- .get(path, config(token=token), verbose)
    invisible(response)
}


#' @rdname leonardo
#'
#' @details `notebooks_invalidate_token()` queries the leonardo
#'     service when a user's Google token is invalidated (e.g. when
#'     logging out of the application). This ensures that the token is
#'     also invalidated in Leo and that the user's proxied notebook
#'     connections stop working.
#'
#' @return `notebooks_invalidate_token()` returns "OK" when the token
#'     is successfully invalidated.
#'
#' @export
notebooks_invalidate_token <-
    function(verbose = FALSE)
{
    path  <- "/notebooks/invalidateToken"
    token <- authenticate()
    .get(path, config(token = token), verbose)
}



##
## status
##

#' @rdname leonardo
#'
#' @details `status()` queries the leonardo service for status of all
#'     system components.
#'
#' @return `status()` returns a JSON list-of-lists describing the status of
#'     the system. It raises a warning of the status is not 'ok'.
#'
#' @export
status <-
    function(verbose = FALSE)
{
    path <- "/status"
    response <- .get(path, verbose = verbose, check = warn_for_status)
    fromJSON(response)
}

##
## test
##


#' @rdname leonardo
#'
#' @details `ping()` tests the accessibility of leonardo.
#'
#' @return `ping()` returns "OK" on success, JSON list-of-lists on on
#'     error.
#'
#' @importFrom httr headers
#' @export
ping <-
    function(verbose = FALSE)
{
    path  <- "/ping"
    response <- .get(
        path, verbose = verbose, content_only = FALSE, check = warn_for_status
    )

    value <- content(response, "text")
    if (startsWith(headers(response)$`content-type`, "application/json"))
        value <- fromJSON(value)
    value
}
