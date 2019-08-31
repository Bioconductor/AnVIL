#' @import futile.logger

.onLoad <-
    function(...)
{
    opts <- list(
        anvil_client_id = "250475993726-k70p3kf2fe2tpuq5jn39ogafbuj9fb8o.apps.googleusercontent.com",
        anvil_client_secret = oauth_secret
    )
    opts <- opts[!names(opts) %in% names(options())]
    options(opts)
}
