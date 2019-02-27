#' @import futile.logger

.onLoad <-
    function(...)
{
    authenticate_ok("leonardo") && { leonardo <<- Leonardo(); TRUE }
    authenticate_ok("terra") && { terra <<- Terra(); TRUE }
    authenticate_ok("dockstore") && { dockstore <<- Dockstore(); TRUE }
}
