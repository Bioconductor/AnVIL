## construct a singleton instance for this service

#' @rdname Service
#'
#' @return `leonardo` represents the API of the Leonard container
#'     deployment service at https://leonardo.dev.anvilproject.org/.
#'
#' @export
leonardo <- NULL # assigned in .onLoad, when credentials are available
