#' @import methods

setOldClass("rapi_api")

setOldClass("request")

#' @importFrom rapiclient get_api get_operations get_schemas
#'
#' @export
.Service <- setClass(
    "Service",
    slots = c(
        service = "character",
        config = "request",
        api = "rapi_api"
    )
)

.service <- function(x) x@service

.config <- function(x) x@config

.service_validate_md5sum_warn <- new.env(parent = emptyenv())

#' @importFrom tools md5sum
#' @importFrom utils download.file
.service_validate_md5sum <-
    function(reference_url, reference_md5sum)
{
    flog.debug("Service reference url: %s", reference_url)
    flog.debug("Service reference md5sum: %s", reference_md5sum)

    if (length(reference_md5sum) == 0L)
        return()

    fl <- tempfile()
    download.file(reference_url, fl, quiet = TRUE)
    md5sum <- md5sum(fl)
    test <-
        identical(unname(md5sum), reference_md5sum) ||
        exists(reference_url, envir = .service_validate_md5sum_warn)
    .service_validate_md5sum_warn[[reference_url]] <- TRUE
    if (!test)
        warning(
            "service version differs from validated version",
            "\n    service url: ", reference_url,
            "\n    observed md5sum: ", md5sum,
            "\n    expected md5sum: ", reference_md5sum
        )
}

#' @rdname Service
#'
#' @name Service
#'
#' @title RESTful service constructor
#'
#' @param service character(1) The `Service` class name, e.g., `"terra"`.
#'
#' @param host character(1) host name that provides the API resource,
#'     e.g., `"api.firecloud.org"`.
#'
#' @param config httr::config() curl options
#'
#' @param authenticate logical(1) use credentials from authentication
#'     service file 'auth.json' in the specified package?
#'
#' @param api_url optional character(1) url location of OpenAPI
#'     `.json` or `.yaml` service definition.
#'
#' @param package character(1) (default `AnVIL`) The package where
#'     'api.json' yaml and (optionally) 'auth.json' files are located.
#'
#' @param schemes character(1) (default 'https') Specifies the
#'     transfer protocol supported by the API service.
#'
#' @param api_reference_url character(1) path to reference API. See
#'     Details.
#'
#' @param api_reference_md5sum character(1) the result of
#'     `tools::md5sum()` applied to the reference API.
#'
#' @details This function creates a RESTful interface to a service
#'     provided by a host, e.g., "api.firecloud.org". The function
#'     requires an OpenAPI `.json` or `.yaml` specifcation as well as
#'     an (optional) `.json` authentication token. These files are
#'     located in the source directory of a pacakge, at
#'     `<package>/inst/service/<service>/api.json` and
#'     `<package>/inst/service/<service>/auth.json`, or at `api_url`.
#'
#'     When provided, the `api_reference_md5sum` is used to check that
#'     the file described at `api_reference_url` has the same checksum
#'     as an author-validated version.
#'
#'     The service is usually a singleton, created at the package
#'     level during `.onLoad()`.
#'
#' @return An object of class \code{Service}.
#'
#' @examples
#' .MyService <- setClass("MyService", contains = "Service")
#'
#' MyService <- function() {
#'     .MyService(Service("my_service", host="my.api.org"))
#' }
#' 
#' @export
Service <-
    function(
        service, host, config = httr::config(), authenticate = TRUE,
        api_url = character(), package = "AnVIL", schemes = "https",
        api_reference_url = api_url,
        api_reference_md5sum = character())
{
    stopifnot(
        .is_scalar_character(service),
        .is_scalar_character(host),
        .is_scalar_logical(authenticate),
        length(api_url) == 0L || .is_scalar_character(api_url),
        length(api_reference_url) == 0L ||
            .is_scalar_character(api_reference_url),
        length(api_reference_md5sum) == 0L ||
            .is_scalar_character(api_reference_md5sum)
    )
    flog.debug("Service(): %s", service)

    .service_validate_md5sum(api_reference_url, api_reference_md5sum)

    if (authenticate)
        config <- c(authenticate_config(service), config)

    withCallingHandlers({
        if (length(api_url)) {
            path <- api_url
        } else {
            path <- .api_path(service, package)
        }
        api <- get_api(path, config)
    }, warning = function(w) {
        test <- identical(
            conditionMessage(w),
            "Missing Swagger Specification version"
        )
        if (!test)
            warning(w)
        invokeRestart("muffleWarning")
    })
    api$schemes <- schemes
    api$host <- host
    api$paths <- .api_paths_fix(api$paths)
    .Service(service = service, config = config, api = api)
}

#' @importFrom utils .DollarNames
#'
#' @export
.DollarNames.Service <-
    function(x, pattern)
{
    grep(pattern, names(operations(x)), value = TRUE)
}

#' @export
setMethod(
    "show", "Service",
    function(object)
{
    cat(
        "service: ", .service(object), "\n",
        "tags(); use ", tolower(class(object)), "$<tab completion>:\n",
        sep = ""
    )
    tbl <- tags(object)
    print(tbl)
    cat(
        "tag values:\n",
        .pretty(unique(tbl$tag), 2, 2), "\n",
        "schemas():\n",
        .pretty(names(schemas(object)), 2, 2, some = TRUE), "\n",
        sep = ""
    )
})
