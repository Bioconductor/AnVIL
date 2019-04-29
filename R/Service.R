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

.api <- function(x) x@api

.api_path <-
    function(service, package)
{
    system.file(
        package = package, "service", service, "api.json", mustWork = TRUE
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
#' @param package character(1) (default `AnVIL`) The package where
#'     'api.json' yaml and (optionally) 'auth.json' files are located
#'
#' @details This function creates a RESTful interface to a service
#'     provided by a host, e.g., "api.firecloud.org". The function
#'     requires an OpenAPI `.json` specifcation as well as an
#'     (optional) `.json` authentication token. These files are
#'     located in the source directory of a pacakge, at
#'     `<package>/inst/service/<service>/api.json` and
#'     `<package>/inst/service/<service>/auth.json`.
#'
#'     The service is usually a singleton, created at the package
#'     level during `.onLoad()`.
#'
#' @return An object of class \code{Service}.
#'
#' @examples
#' \dontrun{
#' .MyService <- setClass("MyService", contains = "Service")
#'
#' MyService <- function() {
#'     .MyService(Service("my_service", host="my.api.org"))
#' }
#' }
#'
#' @export
Service <-
    function(service, host, config = httr::config(), authenticate = TRUE,
             package = "AnVIL")
{
    stopifnot(
        .is_scalar_character(service),
        .is_scalar_character(host),
        .is_scalar_logical(authenticate)
    )
    flog.debug("Service(): %s", service)

    if (authenticate)
        config <- c(authenticate_config(service), config)

    withCallingHandlers({
        path <- .api_path(service, package)
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
    api$schemes <- "https"
    api$host <- host
    .Service(service = service, config = config, api = api)
}

#' @export
setMethod(
    "operations", "Service",
    function(x)
{
    get_operations(.api(x))
})

#' @export
setMethod(
    "schemas", "Service",
    function(x)
{
    get_schemas(.api(x))
})

.operation_field <-
    function(operations, field)
{
    lapply(operations, function(operation) {
        definition <- attr(operation, "definition")
        definition[[field]]
    })
}

#' @rdname Services
#'
#' @name Services
#'
#' @title RESTful services useful for AnVIL developers
#'
#' @aliases .DollarNames.Service operations,Service-method
#'     schemas,Service-method show,Service-method Service-class
NULL

#' @rdname Services
#'
#' @param x A `Service` instance, usually a singleton provided by the
#'     package and documented on this page, e.g., `leonardo` or
#'     `terra`.
#'
#' @param .tags optional character() of tags to use to filter operations.
#'
#' @examples
#' tags(terra)
#' tags(terra, "Billing")
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter arrange
#'
#' @export
tags <-
    function(x, .tags)
{
    operations <- operations(x)
    tags <- .operation_field(operations, "tags")
    summary <- .operation_field(operations, "summary")
    summary <-  trimws(unlist(summary, use.names=FALSE))
    tbl <- tibble(
        tag = unlist(tags, use.names=FALSE),
        operation = rep(names(tags), lengths(tags)),
        summary = rep(summary, lengths(tags))
    )
    if (!missing(.tags))
        tbl <- filter(tbl, tbl$tag %in% .tags)
    arrange(tbl, tbl$tag, tbl$operation)
}

#' @rdname Services
#'
#' @param name A symbol representing a defined operation, e.g.,
#'     `leonardo$listClusters()`.
#'
#' @export
setMethod(
    "$", "Service",
    function(x, name)
{
    operations(x)[[name]]
})

#' @importFrom utils .DollarNames
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
