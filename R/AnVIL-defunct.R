BINARY_BASE_URL <- "https://bioconductor.org/packages/%s/container-binaries/%s"

#' @rdname AnVIL-defunct
#'
#' @name AnVIL-defunct
#'
#' @title Defunct functions in package \sQuote{AnVIL}
#'
#' @description These functions are provided for compatibility with
#'     older versions of \sQuote{AnVIL} only, and will be defunct at
#'     the next release.
#'
#' @param configuration_namespace character(1).
#'
#' @param configuration_name character(1).
#'
#' @param config `avworkflow_configuration` object.
#'
#' @param namespace character(1).
#'
#' @param name character(1).
#'
#' @details The following functions are defunct and will be deleted after the
#'   next Bioconductor release of \sQuote{AnVIL}. Use the replacement indicated
#'   below:
#'
#' - `avworkflow_configuration()`: \code{\link{avworkflow_configuration_get}}
#' - `avworkflow_import_configuration()`: \code{\link{avworkflow_configuration_set}}
avworkflow_configuration <-
    function(configuration_namespace, configuration_name,
             namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    .Defunct("avworkflow_configuration_get", package = "AnVIL")
}

#' @rdname AnVIL-defunct
avworkflow_import_configuration <-
    function(config,
             namespace = avworkspace_namespace(), name = avworkspace_name())
{
    .Defunct("avworkflow_configuration_set", package = "AnVIL")
}

#' @rdname AnVIL-defunct
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
    .Defunct(msg = "'Gen3Fence()' is deprecated, and will be removed")
    Service(
        "gen3/fence", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/fence/master/openapis/swagger.yaml"
    )
}

#' @rdname AnVIL-defunct
#'
#' @return `Gen3Indexd()` returns the indexing service API documented at
#'     https://raw.githubusercontent.com/uc-cdis/indexd/master/openapis/swagger.yaml
#' @format NULL
#'
#' @export
Gen3Indexd <-
    function()
{
    .Defunct(msg = "'Gen3Index()' is deprecated, and will be removed")
    Service(
        "gen3/indexd", "gen3.theanvil.io",
        api_url = "https://raw.githubusercontent.com/uc-cdis/indexd/master/openapis/swagger.yaml"
    )
}

#' @rdname AnVIL-defunct
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
    .Defunct(msg = "'Gen3Sheepdog()' is deprecated, and will be removed")
    Service(
        "gen3/sheepdog", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/sheepdog/master/openapi/swagger.yml"
    )
}

#' @rdname AnVIL-defunct
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
    .Defunct(msg = "'Gen3Peregrine()' is deprecated, and will be removed")
    Service(
        "gen3/peregrine", "FIXME:service_path",
        api_url = "https://raw.githubusercontent.com/uc-cdis/peregrine/master/openapis/swagger.yaml"
    )
}

#' @rdname AnVIL-defunct
#'
#' @title Discover binary packages for fast installation
#'
#' @description `install()` is deprecated in favor of
#'     `BiocManager::install()`.
#'
#' @inheritParams AnVIL-deprecated
#'
#' @param pkgs `character()` packages to install from binary repository.
#'
#' @param ... additional arguments. `install()` passes additional
#'     arguments to
#'     `BiocManager::install()`. `print.repository_stats()` ignores
#'     the additional arguments.
#'
#' @export
install <-
    function(
        pkgs = character(), ...,
        version = BiocManager::version(), binary_base_url = BINARY_BASE_URL
    )
{
    .Defunct("BiocManager::install()", "AnVIL")
}

#' @rdname AnVIL-defunct
#'
#' @aliases BINARY_BASE_URL
#'
#' @description `repository()` is deprecated in favor of
#'     `BiocManager::containerRepository()`.
#'
#' @importFrom utils contrib.url
#'
#' @export
repository <-
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
    .Defunct("BiocManager::containerRepository()")
}

#' @rdname AnVIL-defunct
#'
#' @description `repositories()` is deprecated in favor of
#'     `BiocManager::repositories()`.
#'
#' @export
repositories <-
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
    .Defunct("BiocManager::repositories()")
}
