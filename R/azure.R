## FIXME: use cached values of login, subscription, storage_account

#' @rdname azure
#'
#' @name azure
#'
#' @title azure cloud storage interface
#'
#' @description These functions interface with the Microsoft Azure
#'     cloud using a REST interface; no additional software is
#'     required.
NULL

.is_azure_endpoint_and_container <-
    function(x)
{
    test <- .is_scalar_character(x) && startsWith(x, "https://")
    if (test) {
        fields <- strsplit(x, "/")[[1]]
        test <- test && length(fields) >= 4L && all(nchar(fields)[3:4] > 0L)
    }
    test
}

.azure_choose_first <-
    function(what, choices)
{
    if (length(choices) == 0L)
        stop("no '", what, "' available")
    if (length(choices) > 1L)
        message("more than one '", what, "' available; choosing first")
    head(choices, 1L)
}

#' @rdname azure
#'
#' @description `azure_account()`: authenticate or retrieve a
#'     previously authenticated azure login.
#'
#' @param tenant character(1) tenant to be used during login. Default
#'     value `"common"`.
#'
#' @param account NULL or character(1) account name (typically email
#'     address) for login.
#'
#' @details For `azure_account()`, the default behavior (without
#'     `account=`) opens a web browser prompting for login
#'     credentials.
#'
#' @return `azure_account()` is primarily intended for its side effect
#'     of recording the login credentials of the Azure account. It
#'     returns an authentication object documented in
#'     `?AzureRMR::az_rm`. The object can be used in workflows
#'     supported by the `AzureStor` and other packages.
#'
#' @importFrom AzureRMR list_azure_logins create_azure_login
#'     get_azure_login
#'
#' @export
azure_account <- # gcloud_account
    function(tenant = "common", account = NULL, ...)
{
    stopifnot(
        .is_scalar_character(tenant),
        .is_null_or_scalar_character(account)
    )

    tenants <- names(list_azure_logins())
    if (!tenant %in% tenants)
        create_azure_login(tenant = tenant, username = account, ...)

    get_azure_login(tenant)
}

#' @rdname azure
#'
#' @description `azure_project()`: validate the subscription, resource
#'     group, and storage account to be used for storage access.
#'
#' @param subscription NULL or character(1) Azure subscription name.
#'
#' @param resource_group NULL or character(1) Azure resource group name.
#'
#' @param storage_account NULL or character(1) Azure storage account name.
#'
#' @details `azure_project()` queries the Azure account for access to
#'     the corresponding `subscription`, `resource_group`, and
#'     `storage_account`; `tenant` and `account` determine the login
#'     credentials to use. Suitable values for these parameters can be
#'     discovered by browsing the Azure portal, or by using
#'     functionality in the `AzureStor` package. The default behavior
#'     (invoking `azure_project()` with no arguments) chooses the
#'     'first' login, subscription, resource group, and storage
#'     account returned by queries to underlying `AzureStor`
#'     objects. This behavior is only reliable when a single entity is
#'     available at each level.
#'
#' @return `azure_project()` returns an object representing the
#'     requested storage account, and documented in
#'     `?AzureStor::az_storage`.
#' 
#' @export
azure_project <- # azure subscription and resource group
    function(subscription = NULL, resource_group = NULL, storage_account = NULL,
             tenant = NULL, account = NULL)
{
    stopifnot(
        .is_null_or_scalar_character(subscription),
        .is_null_or_scalar_character(resource_group),
        .is_null_or_scalar_character(storage_account),
        .is_null_or_scalar_character(tenant),
        .is_null_or_scalar_character(account)
    )

    if (is.null(tenant)) {
        logins <- list_azure_logins()
        tenant <- .azure_choose_first("tenant", names(logins))
    }

    azure_account <- azure_account(tenant = tenant)

    if (is.null(subscription)) {
        subscriptions <- azure_account$list_subscriptions()
        id <- .azure_choose_first("subscription", names(subscriptions))
        subscription <- subscriptions[[id]]$name
    }
    subscription <- azure_account$get_subscription_by_name(subscription)

    if (is.null(resource_group)) {
        resource_groups <- subscription$list_resource_groups()
        resource_group <-
            .azure_choose_first("resource_group", names(resource_groups))
    }
    resource_group <- subscription$get_resource_group(resource_group)

    if (is.null(storage_account)) {
        storage_accounts <- resource_group$list_storage_accounts()
        storage_account <-
            .azure_choose_first("storage_account", names(storage_accounts))
    }
    resource_group$get_storage_account(storage_account)
}

.azure_storage_account_sas <-
    function(services = "b", permissions = "rl", resource_types = "sco",
             resource_group = NULL, storage_account = NULL,
             subscription = NULL, tenant = NULL, account = NULL)
{
    storage_account <- azure_project(
        resource_group, storage_account, subscription, tenant, account
    )
    storage_account$get_account_sas(
        services = services,
        permissions = permissions,
        resource_types = resource_types
    )
}

.azure_parse_uri <-
    function(uri)
{
    regexpr <- "(https://[^/]+)/([^/]+)/?(.*)"
    endpoint <- sub(regexpr, "\\1", uri)
    container <- sub(regexpr, "\\2", uri)
    dir <- sub(regexpr, "\\3", uri)
    if (!nzchar(dir))
        dir <- "/"

    list(endpoint = endpoint, container = container, dir = dir)
}

.azure_storage_container <-
    function(parsed_uri, sas)
{
    storage_endpoint <- storage_endpoint(parsed_uri$endpoint, sas = sas)
    storage_container(storage_endpoint, parsed_uri$container)
}

#' @rdname azure
#'
#' @description `azure_ls()`: list the content of the Azure storage
#'     endpoint, container and path implied by the `source` argument.
#'
#' @param source character(1). For `azure_ls(), a URI pointing to an
#'     Azure cloud resource. The URI has form
#'     `https://endpoint/container/path`, discussed in the "Details"
#'     section.
#'
#'     For `azure_cp()`, either a URI or local file or directory path
#'     representing the original location of the object(s) to be
#'     copied.
#'
#' @param destination character(1) a URI or local file (when `source`
#'     is a single object) or folder representing the final location
#'     object(s) are to be copied.
#'
#' @param ... Additional arguments passed to `azure_project()`. The
#'     default (no arguments) uses the default login and project.
#'
#' @param recursive logical(1) peform the operation recursively?
#'
#' @details `azure_ls()` parses a uri of form
#'     `https://endpoint/container/path` defining an Azure storage
#'     object. The uri is adorned with a shared access signature (SAS)
#'     obtained from the storage account identified by the `...`
#'     arguments. `path` is treated as a prefix used to filter all
#'     objects in the storage container; it does not have to fully
#'     resolve to an object. `recursive = FALSE` trims the return
#'     value to objects at the same `/`-delimited level as the path;
#'     these may be objects or simply a portion of the storage path
#'     (typically terminating in `/`). `recursive = TRUE) returns
#'     paths to all objects with prefix `path`.
#'
#' @return `azure_ls()` returns a character vector of object or
#'     partial object paths.
#'
#' @importFrom AzureStor storage_endpoint storage_container
#'     list_storage_files
#'
#' @export
azure_ls <-
    function(source, ..., recursive = FALSE)
{
    stopifnot(
        .is_azure_endpoint_and_container(source),
        .is_scalar_logical(recursive)
    )

    sas <- .azure_storage_account_sas(...)

    parsed_uri <- .azure_parse_uri(source)
    storage_container <- .azure_storage_container(parsed_uri, sas)

    is_dir <- endsWith(parsed_uri$dir, "/")
    if (is_dir) {
        dir <- parsed_uri$dir
    } else {
        dir <- dirname(parsed_uri$dir)
        if (identical(dir, "."))
            dir <- "/"
    }

    blobs <- list_storage_files(
        storage_container,
        dir = dir,
        info = "name",
        recursive = recursive
    )

    if (!is_dir) {
        blobs[startsWith(blobs, parsed_uri$dir)]
    } else {
        blobs
    }
}

.azure_cp_upload <-
    function(source, destination, sas, recursive = FALSE, parallel = TRUE)
{
}

#' @importFrom AzureStor storage_multidownload
.azure_cp_download <-
    function(source, destination, sas, recursive = FALSE, parallel = TRUE)
{
    parsed_uri <- .azure_parse_uri(source)
    storage_container <- .azure_storage_container(parsed_uri, sas)

    blobs <- azure_ls(source, recursive = recursive)
    blobs <- blobs[!endsWith(blobs, "/")] # only blobs downloaded

    if (length(blobs) > 1L) {
        destinations <- file.path(destination, blobs)
        stopifnot(
            dir.exists(destination),
            !any(file.exists(destinations))
        )
    } else {
        destinations <- destination
        stopifnot(
            dir.exists(dirname(destination)),
            !any(file.exists(destinations))
        )
    }
    storage_multidownload(
        storage_container,
        blobs,
        destinations,
        recursive = recursive
    )
}

#' @rdname azure
#'
#' @description `azure_cp()`: copy files to or from Azure storage.
#'
#' @param parallel logical(1) perform the copy in parallel?
#'
#' @details `azure_cp()` supports movement of objects to or from the
#'     Azure cloud. When `source` is an Azure URI, the objects to be
#'     copied are determined by `azure_ls(source, ..., recursive =
#'     recursive)`; using `source = "https://endpoint/container"` and
#'     `recursive = TRUE` replicates the entire container content
#'     locally.
#'
#' @return `azure_cp()` returns `destination`, invisibly.
#'
#' @export
azure_cp <-
    function(source, destination, ..., recursive = FALSE, parallel = TRUE)
{
    ## FIXME: allow both source and destination to be cloud URIs
    ## FIXME: allow vector of sources / destinations
    source_is_uri <- .is_azure_endpoint_and_container(source)
    destination_is_uri <- .is_azure_endpoint_and_container(destination)
    stopifnot(
        source_is_uri || destination_is_uri,
        !(source_is_uri && destination_is_uri),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel)
    )

    sas <- .azure_storage_account_sas(...)

    if (source_is_uri) {
        .AZURE_CP_FUN <- .azure_cp_download
    } else {
        .AZURE_CP_FUN <- .azure_cp_upload
    }
    .AZURE_CP_FUN(source, destination, sas, recursive, parallel)

    invisible(destination)
}
