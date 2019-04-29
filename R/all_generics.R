#' @rdname Services
#'
#' @return `empty_object` returns a representation to be used as
#'     arguments in function calls expecting the empty json object
#'     `\{\}`.
#'
#' @format NULL
#'
#' @examples
#' empty_object
#'
#' @export
empty_object <- setNames(list(), character())

#' @rdname Services
#'
#' @export
setGeneric("operations", function(x) standardGeneric("operations"))

#' @rdname Services
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
