#' @importFrom httr GET PUT POST PATCH DELETE stop_for_status
#'     warn_for_status content

.leonardo_url <-
    function(path)
{
    paste0(anvil_options("leonardo_host"), path)
}

.get <-
    function(path, authorization = NULL, verbose = FALSE, content_only = TRUE,
             check = stop_for_status)
{
    ## Validate args
    stopifnot(.is_scalar_logical(content_only))

    ## GET and check response
    response <- GET(
        .leonardo_url(path), anvil_options("leonardo_config"), accept_json(),
        authorization, if (verbose) verbose()
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

.request <-
    function(FUN, path, authorization = NULL, body = NULL, ...,
             verbose = FALSE, content_only = TRUE, check = stop_for_status)
{
    ## Validate args
    stopifnot(.is_scalar_logical(content_only))

    ## FUN (PUT / POST / PATCH / DELETE) and check response
    response <- FUN(
        .leonardo_url(path), anvil_options("leonardo_config"), body = body,
        authorization, if (verbose) verbose(), encode = "json", ...
    )
    check(response)

    ## Process and return result
    if (content_only) {
        content(response, "text")
    } else response
}

.put <-
    function(path, authorization = NULL, body = NULL, ...,
             verbose = FALSE, content_only = TRUE, check = stop_for_status)
{
    .request(
        PUT, path, authorization, body, ...,
        verbose = verbose, content_only = content_only, check = check
    )
}

.post <-
    function(path, authorization = NULL, body = NULL, ...,
             verbose = FALSE, content_only = TRUE, check = stop_for_status)
{
    .request(
        POST, path, authorization, body, ...,
        verbose = verbose, content_only = content_only, check = check
    )
}

.patch <-
    function(path, authorization = NULL, body = NULL, ...,
             verbose = FALSE, content_only = TRUE, check = stop_for_status)
{
    .request(
        PATCH, path, authorization, body, ...,
        verbose = verbose, content_only = content_only, check = check
    )
}

.delete <-
    function(path, authorization = NULL, body = NULL, ...,
             verbose = FALSE, content_only = TRUE, check = stop_for_status)
{
    .request(
        DELETE, path, authorization, body, ...,
        verbose = verbose, content_only = content_only, check = check
    )
}
