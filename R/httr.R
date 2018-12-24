#' @importFrom httr GET PUT POST PATCH DELETE stop_for_status
#'     warn_for_status content

.get <-
    function(path, authorization = NULL, verbose = FALSE, content_only = TRUE,
             check = stop_for_status)
{
    stopifnot(
        .is_scalar_logical(verbose),
        .is_scalar_logical(content_only)
    )
    url <- paste0(anvil_options("leonardo_host"), path)

    response <- GET(
        url, anvil_options("leonardo_config"), accept_json(), authorization,
        if (verbose) verbose()
    )
    check(response)

    if (content_only) {
        content(response, "text")
    } else response
}

.put <-
    function()
{
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
