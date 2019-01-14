.onLoad <-
    function(...)
{
    path <- .authenticate_path("leonardo")
    if (!file.exists(path)) {
        warning(
            "AnVIL Leonardo requires additional configuration; ",
            "see `?authenticate`"
        )
    } else {
        leonardo <<- Service(
            "leonardo",
            host = "leonardo.dev.anvilproject.org",
            config = httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
        )
    }
}        
