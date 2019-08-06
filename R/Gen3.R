#' @rdname Services
#'
#' @return `gen3_*` APIs are not fully implemented, because a service
#'     endpoint has not been identifiied.
#'
#'     `gen3_fence` provides authentication at
#'     https://raw.githubusercontent.com/uc-cdis/fence/master/openapis/swagger.yaml
#' @format NULL
#'
#' @export
gen3_fence <- NULL

Gen3Fence <-
    function()
{
    Service(
        "gen3/fence", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/fence/master/openapis/swagger.yaml"
    )
}

#' @rdname Services
#'
#' @return `gen3_indexd` provides an indexing service documented at
#'     https://raw.githubusercontent.com/uc-cdis/indexd/master/openapis/swagger.yaml
#' @format NULL
#'
#' @export
gen3_indexd <- NULL

Gen3Indexd <-
    function()
{
    Service(
        "gen3/indexd", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/indexd/master/openapis/swagger.yaml"
    )
}

#' @rdname Services
#'
#' @return `gen3_sheepdog` provides submission services at
#'     https://raw.githubusercontent.com/uc-cdis/sheepdog/master/openapi/swagger.yml
#'
#' @format NULL
#'
#' @export
gen3_sheepdog <- NULL

Gen3Sheepdog <-
    function()
{
    Service(
        "gen3/sheepdog", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/sheepdog/master/openapi/swagger.yml"
    )
}

#' @rdname Services
#'
#' @return `gen3_peregrine` provides graphQL query services at
#'     https://raw.githubusercontent.com/uc-cdis/peregrine/master/openapis/swagger.yaml
#'
#' @format NULL
#'
#' @export
gen3_peregrine <- NULL

Gen3Peregrine <-
    function()
{
    Service(
        "gen3/peregrine", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/peregrine/master/openapis/swagger.yaml"
    )
}
