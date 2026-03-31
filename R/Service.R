#' @import methods

setOldClass("rapi_api")

setOldClass("request")

#' @importFrom rapiclient get_api
#'
#' @export
.Service <- setClass(
    "Service",
    slots = c(
        service = "character",
        config = "request",
        api = "rapi_api",
        host = "character"
    )
)

.service <- function(x) x@service
.host <- function(x) x@host
.config <- function(x) x@config

#' @importFrom httr write_disk GET add_headers
#' @importFrom AnVILBase avstop_for_status
.service_get_api_file <- function(reference_url, reference_headers) {
    fl <- tempfile()
    response <- GET(
        reference_url,
        add_headers(.headers = reference_headers),
        write_disk(fl)
    )
    avstop_for_status(response, ".service_get_api_file")
    fl
}

.service_validate_sha256_warn <- new.env(parent = emptyenv())

.service_validate_sha256 <-
    function(reference_url, reference_sha256, reference_headers, api_file)
{
    flog.debug("Service reference url: %s", reference_url)
    flog.debug("Service reference sha256: %s", reference_sha256)

    if (length(reference_sha256) == 0L)
        return()

    sha256 <- digest::digest(api_file, algo = "sha256", file = TRUE)
    test <-
        identical(unname(sha256), reference_sha256) ||
        exists(reference_url, envir = .service_validate_sha256_warn)
    .service_validate_sha256_warn[[reference_url]] <- TRUE
    if (!test)
        warning(
            "service version differs from validated version",
            "\n    service url: ", reference_url,
            "\n    observed sha256: ", sha256,
            "\n    expected sha256: ", reference_sha256
        )
    test
}

.service_read_version <- function(file) {
    yaml_file <- yaml::read_yaml(file)
    yaml_file[["info"]][["version"]]
}

.service_validate_version <-
    function(reference_url, reference_version, reference_headers, api_file)
{
    flog.debug("Service reference url: %s", reference_url)
    flog.debug("Service reference version: %s", reference_version)

    if (!length(reference_version))
        return()

    version <- .service_read_version(api_file)

    if (!length(version))
        return()

    test <- identical(version, reference_version)
    if (!test)
        warning(
            "service version differs from validated version",
            "\n    service url: ", reference_url,
            "\n    observed version: ", version,
            "\n    expected version: ", reference_version
        )
    test
}

#' @rdname Service
#'
#' @name Service
#'
#' @title RESTful service constructor
#'
#' @param service `character(1)` The `Service` class name, e.g., `"terra"`.
#'
#' @param host `character(1)` host name that provides the API resource,
#'     e.g., `"leonardo.dsde-prod.broadinstitute.org"`.
#'
#' @param config httr::config() curl options
#'
#' @param authenticate `logical(1)` use credentials from authentication
#'     service? See `?anvil_set_auth_json` for the recommended way to
#'     securely store credentials.
#'
#' @param api_url optional `character(1)` url location of OpenAPI
#'     `.json` or `.yaml` service definition.
#'
#' @param package `character(1)` (default `AnVIL`) The package where
#'     'api.json' yaml is located.
#'
#' @param schemes `character(1)` (default 'https') Specifies the
#'     transfer protocol supported by the API service.
#'
#' @param api_reference_url `character(1)` path to reference API. See
#'     Details.
#'
#' @param api_reference_md5sum `character(1)` the result of
#'     `tools::md5sum()` applied to the reference API.
#'
#' @param api_reference_version `character(1)` the version of the
#'    reference API. This is used to check that the version of the
#'    service matches the version of the reference API. It is usally
#'    set by the service generation function,. e.g., `AnVIL::Rawls()`.
#'
#' @param api_reference_headers `character()` header(s) to be used
#'     (e.g., `c(Authorization = paste("Bearer", token))`) when
#'     retrieving the API reference for validation.
#'
#' @param ... additional arguments passed to `rapiclient::get_api()`
#'
#' @details This function creates a RESTful interface to a service
#'     provided by a host, e.g., "leonardo.dsde-prod.broadinstitute.org".
#'     The function requires an OpenAPI `.json` or `.yaml` specification.
#'     The specification file is located in the source directory of a
#'     package, at `<package>/inst/service/<service>/api.json`, or at
#'     `api_url`.
#'
#'     Authentication credentials can be stored securely in the system
#'     keyring using `anvil_set_auth_json()`.
#'
#' When provided, the `api_reference_md5sum` is used to check that
#' the file described at `api_reference_url` has the same checksum
#' as an author-validated version.
#'
#' The service is usually a singleton, created at the package
#' level during `.onLoad()`.
#'
#' @returns An object of class \code{Service}.
#'
#' @importFrom BiocBaseUtils isScalarCharacter isScalarLogical isCharacter
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
        api_reference_md5sum = character(),
        api_reference_version = character(),
        api_reference_headers = NULL,
        ...
) {
    stopifnot(
        isScalarCharacter(service),
        isScalarCharacter(host),
        isScalarLogical(authenticate),
        length(api_url) == 0L || isScalarCharacter(api_url),
        length(api_reference_url) == 0L ||
            isScalarCharacter(api_reference_url),
        length(api_reference_md5sum) == 0L ||
            isScalarCharacter(api_reference_md5sum),
        length(api_reference_version) == 0L ||
            isScalarCharacter(api_reference_version),
        is.null(api_reference_headers) || isCharacter(api_reference_headers)
    )
    flog.debug("Service(): %s", service)

    api_file <- .service_get_api_file(api_reference_url, api_reference_headers)

    .service_validate_sha256(
        api_reference_url, api_reference_md5sum,
        api_reference_headers, api_file
    )

    .service_validate_version(
        api_reference_url, api_reference_version,
        api_reference_headers, api_file
    )

    if (authenticate)
        config <- c(authenticate_config(service), config)

    withCallingHandlers({
        if (length(api_url)) {
            path <- api_url
        } else {
            path <- .api_path(service, package)
        }
        api <- get_api(path, config, ...)
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
    .Service(service = service, config = config, api = api, host = host)
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
        "host: ", .host(object), "\n",
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
