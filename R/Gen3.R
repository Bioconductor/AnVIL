#' @rdname Service
#'
#' @return `gen3_*` APIs are not fully implemented, because a service
#'     endpoint has not been identifiied.
#'
#'     `gen3_fence` provides authentication at
#'     https://raw.githubusercontent.com/uc-cdis/fence/master/openapis/swagger.yaml
#' @export
gen3_fence <- Service("gen3/fence", "FIXME:service_path")

#' @rdname Service
#'
#' @return `gen3_indexd` provides an indexing service documented at
#'     https://raw.githubusercontent.com/uc-cdis/indexd/master/openapis/swagger.yaml
#' @export
gen3_indexd <- Service("gen3/indexd", "FIXME:service_path")

#' @rdname Service
#'
#' @return `gen3_sheepdog` provides submission services at
#'     https://raw.githubusercontent.com/uc-cdis/sheepdog/master/openapi/swagger.yml
#'
#' @export
gen3_sheepdog <- Service("gen3/sheepdog", "FIXME:service_path")

#' @rdname Service
#'
#' @return `gen3_peregrine` provides graphQL query services at
#'     https://raw.githubusercontent.com/uc-cdis/peregrine/master/openapis/swagger.yaml
#'
#' @export
gen3_peregrine <- Service("gen3/peregrine", "FIXME:service_path")
