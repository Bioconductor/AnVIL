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
    type <- ifelse(
        is.null(op_def$consumes), "application/json", op_def$consumes
    )
    content_type(type)
}

#' @importFrom httr accept
.api_get_accept <-
    function(op_def)
{
    ## claim that we will accept anything. OAS3.0 allows responses to
    ## support different content; our 2.x converter assumes the most
    ## specific (e.g., application/json), but then some responses
    ## (e.g., text/plain) generate errors.
    type <- "*/*"
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
    function(op_def, body, auto_unbox)
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
        json <- jsonlite::toJSON(body, pretty = TRUE, auto_unbox = auto_unbox)
    }

    json
}

#' @importFrom rapiclient get_operation_definitions
#' @importFrom httr POST PATCH PUT GET HEAD DELETE
.api_get_operations <-
    function(api, .headers = NULL, path = NULL, handle_response = identity,
        auto_unbox = FALSE
    )
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
                body <-  .api_get_message_body(op_def, body0, auto_unbox)
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
        if (what %in% c("POST", "PATCH", "PUT")) {
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

## The following functions are for use by tests/testthat/test_api.R

.api_test_file_path <-
    function(name)
{
    devtools::package_file("tests", "testthat", paste0("api-", name, ".rds"))
}

.api_test_in_use <- function(function_names, service_name, r_content) {
    in_use <- vapply(
        function_names, function(function_name, service_name, r_content) {
            ## FIXME: fragile
            function_call <- paste0(service_name, "()$", function_name, "(")
            any(grepl(function_call, r_content, fixed = TRUE))
        }, logical(1), service_name, r_content)
    function_names[in_use]
}

.api_test_write <-
    function(service, name)
{
    file_path <- .api_test_file_path(name)
    ops <- lapply(operations(service), formals)
    saveRDS(ops, file_path)
}

.api_test_check <-
    function(service, name)
{
    ops <- lapply(operations(service), formals)
    ops_saved <- readRDS(.api_test_file_path(name))

    common <- intersect(names(ops), names(ops_saved))
    added <- setdiff(names(ops), names(ops_saved))
    removed <- setdiff(names(ops_saved), names(ops))

    is_updated <- vapply(common, function(op_name) {
        !identical(ops_saved[[op_name]], ops[[op_name]])
    }, logical(1))
    updated <- common[is_updated]

    r_files <- dir(
        devtools::package_file("R"), full.names = TRUE, pattern = ".R$"
    )
    r_content <- unlist(lapply(r_files, readLines))

    common_in_use <- .api_test_in_use(common, name, r_content)
    removed_in_use <- .api_test_in_use(removed, name, r_content)
    updated_in_use <- .api_test_in_use(updated, name, r_content)

    list(
        common = common, added = added, removed = removed, updated = updated,
        common_in_use = common_in_use,
        removed_in_use = removed_in_use,
        updated_in_use = updated_in_use
    )
}
