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
