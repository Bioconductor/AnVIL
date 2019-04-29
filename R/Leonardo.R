## construct a singleton instance for this service

#' @rdname Services
#'
#' @return `leonardo` represents the API of the Leonard container
#'     deployment service at https://leonardo.dev.anvilproject.org/.
#'
#' @format NULL
#'
#' @export
leonardo <- NULL # assigned in .onLoad, when credentials are available

Leonardo <-
    function()
{
    Service(
        "leonardo",
        host = "leonardo.dev.anvilproject.org",
        config = httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
    )
}
