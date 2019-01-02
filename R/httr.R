#' @importFrom httr GET PUT POST PATCH DELETE stop_for_status
#'     warn_for_status content

.get <-
    function(path, authorization = NULL, verbose = FALSE, content_only = TRUE,
             check = stop_for_status)
{
    ## Validate args
    stopifnot(
        .is_scalar_logical(verbose),
        .is_scalar_logical(content_only)
    )

    ## Compose request
    url <- paste0(anvil_options("leonardo_host"), path)

    ## GET and check response
    response <- GET(
        url, anvil_options("leonardo_config"), accept_json(), authorization,
        if (verbose) verbose()
    )
    check(response)

    ## Process and return result
    if (content_only) {
        content(response, "text")
    } else response
}


## example PUT call,
## PUT(url, anvil_options("leonardo_config"), body = list(biocImage =
## "us.gcr.io/anvil-leo-dev/anvil_bioc_docker:latest"), encode="json",
## config(token=x))

.put <-
    function(path, authorization = NULL, body = NULL, verbose = FALSE,
             content_only = TRUE, check = stop_for_status)
{
    ## Validate args
    stopifnot(
        .is_scalar_logical(verbose),
        .is_scalar_logical(content_only)
    )

    ## compose request
    url <- paste0(anvil_options("leonardo_host"), path)

    ## PUT and check response
    response <- PUT(
        url, anvil_options("leonardo_config"), body = body,
        encode = "json", authorization, if (verbose) verbose()
    )
    check(response)

    ## Process and return result
    if (content_only) {
        content(response, "text")
    } else response
}


.post <-
    function()
{
}

.patch <-
    function()
{
}

.delete <-
    function()
{
}
