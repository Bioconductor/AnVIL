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
#' @param apiheaders a character vector representing headers needed for
#'	the api
#'
#' @param name A symbol representing a defined operation, e.g.,
#'     `leonardo$listClusters()`.
#'
#' @param op - an api operation from rapiclient::get_operations
#'
#' @import methods
NULL

#' @importFrom rapiclient get_api get_operations get_schemas

.tagList <- setClass(
  "tagList",
  slots = c(tags="list")
)

.tags <- function(x) {x@tagList}

.getTags<-function(op){
  temp=attr(op,"definition")
  return(data.frame(operation=temp$operationId,tag=temp$tags, stringsAsFactors=FALSE))
}

tagList<-function(service){
  fpath=.api_path(service)
  myapi=rapiclient::get_api(fpath)
  myops=rapiclient::get_operations(myapi)
  tagDF=do.call("rbind.data.frame",lapply(myops,.getTags))
  tagList=split(tagDF,tagDF$tag)
  return(.tagList(tags=tagList))
}

#' @export
setMethod(
  "show", "tagList",
  function(object)
{str(object@tags,1)}
)

#' @export
setMethod(
  "$", "tagList",
  function(x, name)
  {x@tags[[name]]}
)


#' @export

setOldClass("rapi_api")

setOldClass("request")


#' @export
.Service <- setClass(
    "Service",
    slots = c(
        service = "character",
        config = "request",
        api = "rapi_api",
	apiheaders="character",
	tagList="tagList"
    )
)

.service <- function(x) x@service

.config <- function(x) x@config

.api <- function(x) x@api

.apiheaders<-function(x) x@apiheaders

.api_path <- function(service)
    system.file(package="AnVIL", "service", service, "api.json")

Service <-
    function(service, host, config = httr::config(),apiheaders=character(0))
{
    stopifnot(
        .is_scalar_character(service),
        .is_scalar_character(host)
    )
    flog.debug("Service(): %s", service)

    config <- c(authenticate_config(service), config)

    withCallingHandlers({
        api <- get_api(.api_path(service), config)
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
    tempTagList=tagList(service)
    .Service(service = service, config = config, api = api, apiheaders=apiheaders, tagList=tempTagList)
}

#' @export
setMethod(
    "operations", "Service",
    function(x)
{
    get_operations(.api(x),.headers=.apiheaders(x))
})

#' @export
setMethod(
    "schemas", "Service",
    function(x)
{
    get_schemas(.api(x))
})

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
        "operations() or ", tolower(class(object)), "$<tab completion> :\n",
        .pretty(names(operations(object)), 2, 2), "\n",
        "schemas():\n",
        .pretty(names(schemas(object)), 2, 2), "\n",
        sep=""
    )
})

#' @export
setMethod(
  "tags", "Service",
  function(x)
  {x@tagList}
)
