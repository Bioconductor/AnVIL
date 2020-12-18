.CLASS_OPERATION <- "rapi_operation"

.api <-
    function(x)
{
    x@api
}

.api_path <-
    function(service, package)
{
    fl <- system.file(package = package, "service", service, "api.json")
    if (!file.exists(fl))
        fl <- system.file(package = package, "service", service, "api.yaml")
    if (!file.exists(fl))
        stop("could not find api.json or api.yaml for service '", service, "'")
    fl
}

.api_paths_fix <-
    function(x)
{
    ## 'produces' needs to be character(1) for httr 1.4.1
    if ("produces" %in% names(x))
        x[["produces"]] <- paste(x[["produces"]], collapse = ", ")
    else if (is.list(x))
        x <- lapply(x, .api_paths_fix)
    x
}

.api_args <-
    function(formals, environment)
{
    arg_names <- if (is.null(names(formals))) character() else names(formals)
    arg_names <- arg_names[!arg_names %in% c("...", ".__body__")]
    args <- mget(arg_names, environment)
    args[!vapply(args, is.null, logical(1))]
}

.api_body <-
    function(formals, ..., .__body__)
{
    body0 <- formals$`.__body__`
    dot_args <- list(...)
    .__body__ <- .__body__[!vapply(.__body__, is.null, logical(1))]
    stopifnot(
        all(names(dot_args) %in% names(body0)),
        all(names(body) %in% names(body0)),
        `duplicate values for some '.__body__' arguments` =
            !any(names(dot_args) %in% names(.__body__))
    )
    body <- c(dot_args, .__body__)

    ## always named
    if (is.null(names(body)))
        names(body) <- rep("", length(body))

    ## positional matching of '...' and .__body__ args
    idx <- nzchar(names(body)) # named arguments
    available <- setdiff(names(body0), names(body)[idx])
    names(body)[!idx] <- available[seq_len(sum(!idx))]

    body
}

.api_get_url <-
    function(api, op_def, x)
{
    rapiclient:::build_op_url(
        api, api$schemes[1], api$host, api$basePath, op_def, x
    )
}

.api_get_config <-
    function(api)
{
    api$config
}

#' @importFrom httr content_type
.api_get_content_type <-
    function(op_def)
{
    type <- ifelse(is.null(op_def$consumes), "application/json", op_def$consumes)
    content_type(type)
}

#' @importFrom httr accept
.api_get_accept <-
    function(op_def)
{
    type <- ifelse(is.null(op_def$produces), "*/*", op_def$produces)
    accept(type)
}


.api_is_message_body_parameter <-
    function(op_def, x)
{
    parameters <- op_def$parameters
    vapply(parameters, function(parameter) {
        parameter[["in"]] %in% c("body", "formData")
    }, logical(1))
}

#' @importFrom jsonlite unbox
.api_get_message_body <-
    function(op_def, body)
{
    if (identical(op_def$consumes, "multipart/form-data")) {
        json <- body
    } else {
        if (length(body) == 1L)
            body <- body[[1]]
        ## unbox?
        name <- vapply(op_def$parameters, `[[`, character(1), "name")
        type <- vapply(op_def$parameters, function(elt) {
            type <- elt$type
            if (is.null(type)) {
                ## FIXME: recurse into $ref
                NA_character_
            } else {
                type
            }
        }, character(1))
        for (nm in names(body)) {
            idx <- match(nm, name)
            if (type[idx] %in% c("string", "number", "integer", "boolean"))
                body[[nm]] <- unbox(body[[nm]])
        }
        json <- jsonlite::toJSON(body, pretty = TRUE)
    }

    json
}

#' @importFrom rapiclient get_operation_definitions
#' @importFrom httr POST PATCH PUT GET HEAD DELETE
.api_get_operations <-
    function(api, .headers = NULL, path = NULL, handle_response = identity)
{
    operation_defs <- get_operation_definitions(api, path)

    lapply(operation_defs, function(op_def){
        what <- toupper(op_def$action)
        if (!what %in% c("POST", "PATCH", "PUT", "GET", "HEAD", "DELETE"))
            stop("unsupported REST operation '", what, "'")
        HTTR_FUN <- get(what, envir = getNamespace("httr"))
        FUN <- switch(
            what,
            POST =,
            PATCH =,
            PUT = function(..., .__body__ = list()) {
                args <- .api_args(formals(), environment())
                body0 <- .api_body(formals(), ..., .__body__ = .__body__)
                body <-  .api_get_message_body(op_def, body0)
                result <- HTTR_FUN(
                    url = .api_get_url(api, op_def, args),
                    config = .api_get_config(api),
                    .api_get_content_type(op_def),
                    .api_get_accept(op_def),
                    add_headers(.headers = .headers),
                    body = body
                )
                handle_response(result)
            },
            GET =,
            HEAD =,
            DELETE = function(...) {
                args <- .api_args(formals(), environment())
                result <- HTTR_FUN(
                    url = .api_get_url(api, op_def, args),
                    config = .api_get_config(api),
                    .api_get_content_type(op_def),
                    .api_get_accept(op_def),
                    add_headers(.headers = .headers)
                )
                handle_response(result)
            }
        )

        ## create function arguments from operation parameters definition
        parameters <- rapiclient:::get_parameters(api, op_def$parameters)
        idx <- .api_is_message_body_parameter(op_def, parameters)
        args <- do.call("alist", parameters[!idx])
        if (any(idx)) {
            alist0 <- do.call("alist", list(.__body__ = parameters[idx]))
            args <- c(args, alist(...=), alist0)
        }
        formals(FUN) <- args

        ## add the complete operation definition as a function attribute
        attr(FUN, "definition") <- op_def
        class(FUN) <- c(.CLASS_OPERATION, class(FUN))
        FUN
    })
}
