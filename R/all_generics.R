#' @rdname Service
#'
#' @export
setGeneric("operations", function(x) standardGeneric("operations"))

#' @rdname Service
#'
#' @export
setGeneric("schemas", function(x) standardGeneric("schemas"))

#' @rdname Response
#'
#' @name Response
#'
#' @title Process service responses to tibble and other data structures.
#'
#' @aliases flatten
#'
#' @param x A `response` object returned by the service.
#'
#' @return `flatten()` returns a `tibble` where each row correseponds
#'     to a top-level list element of the return value, and columns
#'     are the unlisted second and more-nested elements.
#'
#' @examples
#' \donttest{leonardo$listClusters() %>% flatten()}
#' 
#' @export
setGeneric("flatten", function(x) standardGeneric("flatten"))
