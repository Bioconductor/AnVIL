#' @rdname Service
#'
#' @name Service
#'
#' @title RESTful services useful for AnVIL developers
#'
#' @aliases .DollarNames.Service operations,Service-method
#'     schemas,Service-method show,Service-method Service-class
#'
#' @param x A `Service` instance, usually a singleton provided by the
#'     package and documented on this page, e.g., `leonardo` or
#'     `terra`.
#'
#' @param service The `Service`` class name
#'
#' @param name A symbol representing a defined operation, e.g.,
#'     `leonardo$listClusters()`.
#'
#' @param config httr::config() curl options
#'
#' @param authenticate_config logical(1L) whether to use authentication service
#'     file 'auth.json' in the specified package
#'
#' @param host The host name that provides the API resource
#'
#' @param package (default `AnVIL`) The package where 'api.json' yaml and
#'     (optionally) 'auth.json' files are located
#'
#' @format An object of class \code{Service} of length 1.
#'
#' @import methods
NULL

setOldClass("rapi_api")

setOldClass("request")

#' @importFrom rapiclient get_api get_operations get_schemas
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

.api_path <- function(service, pkg)
    system.file(package = pkg, "service", service, "api.json", mustWork = TRUE)

#' @export
Service <-
    function(service, host, config = httr::config(),
        authenticate_config = TRUE, package = "AnVIL")
{
    stopifnot(
        .is_scalar_character(service),
        .is_scalar_character(host),
        .is_scalar_logical(authenticate_config)
    )
    flog.debug("Service(): %s", service)

    if (authenticate_config)
        config <- c(authenticate_config(service), config)

    withCallingHandlers({
        api <- get_api(.api_path(service, pkg = package), config)
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

#' @rdname Service
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

#' @rdname Service
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
