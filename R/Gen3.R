#' @rdname Services
#'
#' @return `gen3_*` APIs are not fully implemented, because a service
#'     endpoint has not been identified.
#'
#' @return `Gen3Fence()` returns the authentication API at
#'     https://raw.githubusercontent.com/uc-cdis/fence/master/openapis/swagger.yaml
#' @format NULL
#'
#' @export
Gen3Fence <-
    function()
{
    .Deprecated(msg = "'Gen3Fence()' is deprecated, and will be removed")
    Service(
        "gen3/fence", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/fence/master/openapis/swagger.yaml"
    )
}

#' @rdname Services
#'
#' @return `Gen3Indexd()` returns the indexing service API documented at
#'     https://raw.githubusercontent.com/uc-cdis/indexd/master/openapis/swagger.yaml
#' @format NULL
#'
#' @export
Gen3Indexd <-
    function()
{
    .Deprecated(msg = "'Gen3Index()' is deprecated, and will be removed")
    Service(
        "gen3/indexd", "gen3.theanvil.io",
        api_url = "https://raw.githubusercontent.com/uc-cdis/indexd/master/openapis/swagger.yaml"
    )
}

#' @rdname Services
#'
#' @return `Gen3Sheepdog` returns the submission services API at
#'     https://raw.githubusercontent.com/uc-cdis/sheepdog/master/openapi/swagger.yml
#'
#' @format NULL
#'
#' @export
Gen3Sheepdog <-
    function()
{
    .Deprecated(msg = "'Gen3Sheepdog()' is deprecated, and will be removed")
    Service(
        "gen3/sheepdog", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/sheepdog/master/openapi/swagger.yml"
    )
}

#' @rdname Services
#'
#' @return `Gen3Peregrine` returns the graphQL query services API at
#'     https://raw.githubusercontent.com/uc-cdis/peregrine/master/openapis/swagger.yaml
#'
#' @format NULL
#'
#' @export
Gen3Peregrine <-
    function()
{
    .Deprecated(msg = "'Gen3Peregrine()' is deprecated, and will be removed")
    Service(
        "gen3/peregrine", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/peregrine/master/openapis/swagger.yaml"
    )
}
