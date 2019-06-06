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
    exts <- c("json", "yaml", "yml")
    for (ext in exts) {
        fl <- paste("api", ext, sep=".")
        result <- tryCatch({
            system.file(
                package = package, "service", service, fl, mustWork = TRUE
            )
        }, error = identity)
        if (!is(result, "simpleError"))
            break
    }
    if (is(result, "simpleError"))
        stop(result)
    paste0("file://", result)
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
