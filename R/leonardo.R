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
#' @details `api_update_cluster()` updates the configuration of a
#'     cluster. In order to update the configuration of a cluster, it
#'     must first be running, check with `api_clustes()`.
#'
#' @param googleProject character(1) name of google project, e.g,
#'     `"anvil-leo-dev"`
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#' @param updateClusterRequest list(1) request to a cluster resource in the
#'     form of list(requestName = updateClusterRequest).
#'     Eg: list("machineConfig" = list("numberOfWorkers" = 0,
#'                                     "numberOfPreemptibleWorkers" = 0),
#'              "autopause" = true,
#'              "autopauseThreshold" = 0)
#'
#' @return `api_create_cluster()` returns a json list-of-lists
#'     describing the updated cluster.
#'
#' @export
api_update_cluster <-
    function(googleProject, clusterName, updateClusterRequest)
{
    stopifnot(
        .is_scalar_character(googleProject),
        .is_scalar_character(clusterName),
        is.list(updateClusterRequest)
    )

    path <- sprintf("/api/cluster/%s/%s", googleProject, clusterName)
    token <- authenticate()
    response <- .patch(path, config(token = token), body = updateClusterRequest,
                       verbose = verbose)
    fromJSON(response)
}

#' @rdname leonardo
#'
#' @details `api_delete_cluster()` deletes an existing Dataproc
#'     cluster in the given project
#'
#' @param googleProject character(1) name of google project, e.g,
#'     `"anvil-leo-dev"`
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#' @return character(1) response body is a string.
#'
#' @export
api_delete_cluster <-
    function(googleProject, clusterName)
{
    stopifnot(
        .is_scalar_character(googleProject),
        .is_scalar_character(clusterName)
    )

    path <- sprintf("/api/cluster/%s/%s", googleProject, clusterName)
    token <- authenticate()
    response <- .delete(path, config(token = token), verbose = verbose)
    response
}

##
## This implements only the v2 of the API which creates the dataproc
## cluster in a given project with a given name. Doesn't implement
## https://leonardo.dev.anvilproject.org/#!/cluster/createCluster,
## because the standalone doc doesn't use it.
##

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


#' @rdname leonardo
#'
#' @details `api_cluster_start()` starts a Dataproc cluster
#'
#' @param googleProject character(1) name of google project, e.g,
#'     `"anvil-leo-dev"`
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#'
#' @return
#'
#' @export
api_cluster_start <-
    function(googleProject, clusterName, verbose = FALSE)
{
    stopifnot(
        .is_scalar_character(googleProject),
        .is_scalar_character(clusterName)
    )

    path <- sprintf("/api/cluster/%s/%s/start", googleProject, clusterName)
    token <- authenticate()
    response <- .post(path, config(token = token), verbose = verbose)
    fromJSON(response)
}


#' @rdname leonardo
#'
#' @details `api_cluster_stop()` stops a Dataproc cluster .The
#'     cluster may be restarted with the `api_cluster_start()` endpoint
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
api_cluster_stop <-
    function(googleProject, clusterName, verbose = FALSE)
{
    stopifnot(
        .is_scalar_character(googleProject),
        .is_scalar_character(clusterName)
    )

    path <- sprintf("/api/cluster/%s/%s/stop", googleProject, clusterName)
    token <- authenticate()
    response <- .post(path, config(token = token), verbose = verbose)
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
    function(googleProject, clusterName, verbose = FALSE)
{
    path <- sprintf("/notebooks/%s/%s", googleProject, clusterName)
    token <- authenticate()
    response <- .get(path, config(token=token), verbose = verbose)
    fromJSON(response)
}


#' @rdname leonardo
#'
#' @details `notebooks_api_localize()` localize files to/from a
#'     Jupyter notebook server.Sends a command to a Jupyter notebook
#'     server to localize files to/from the server. Supports GCS paths
#'     and Data URIs. Output, including any errors, will appear in
#'     localization.log in the working directory of the Jupyter
#'     notebook server. By default this operation will happen
#'     synchronously and the response status will reflect any errors
#'     encountered in the copy. However, if the async parameter is
#'     specfied then the localization will happen asynchronously to
#'     the request, and the API will always return 200.
#'
#' @param googleProject character(1) name of google project, e.g,
#'     `"anvil-leo-dev"`
#'
#' @param clusterName character(1) name of a cluster to query for
#'     details. Names are from `api_clusters()$labels.clusterName)`.
#'
#' @param async logical(1) if TRUE, the copy will happen
#'     asynchronously to the request and the API will always return
#'     200. If FALSE (the default), the copy will happen synchronously
#'     and the response will reflect any errors encountered during the
#'     copy.
#'
#' @param filesToLocalize list(1)
#'
#' @return
#'
#' @export
notebooks_api_localize <-
    function(googleProject, clusterName, async = FALSE, filesToLocalize)
{
    stopifnot(
        .is_scalar_character(googleProject),
        .is_scalar_character(clusterName),
        is.logical(async),
        is.list(filesToLocalize)
    )

    path <- sprintf("/notebooks/%s/%s/api/localize", googleProject, clusterName)
    token <- authenticate()
    response <- .post(path, config(token = token), body = filesToLocalize,
                      verbose = verbose, query = async)
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
    .get(path, config(token = token), verbose = verbose)
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
