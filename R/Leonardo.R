## construct a singleton instance for this service

#' @rdname Services
#'
#' @return `leonardo` represents the API of the Leonard container
#'     deployment service at https://leonardo.dev.anvilproject.org/.
#'
#' @format NULL
#'
#' @export
Leonardo <-
    function()
{
    Service(
        "leonardo",
        host = "notebooks.firecloud.org",
        config = httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L),
        api_url = "https://notebooks.firecloud.org/api-docs.yaml"
    )
}
