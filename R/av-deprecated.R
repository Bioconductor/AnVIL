#' @name av-deprecated
#'
#' @aliases avbucket avfiles_ls avfiles_backup avfiles_restore avfiles_rm
#'
#' @title Deprecated higher level functions that use `gsutil`
#'
#' @description `avbucket()` returns the workspace bucket, i.e., the
#'     google bucket associated with a workspace. Bucket content can
#'     be visualized under the 'DATA' tab, 'Files' item.
#'
#' @return `avbucket()` returns a `character(1)` bucket identifier,
#'     prefixed with `gs://` if `as_path = TRUE`.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     ## From within AnVIL...
#'     bucket <- avbucket()                        # discover bucket
#'
#' \dontrun{
#' path <- file.path(bucket, "mtcars.tab")
#' gsutil_ls(dirname(path))                    # no 'mtcars.tab'...
#' write.table(mtcars, gsutil_pipe(path, "w")) # write to bucket
#' gsutil_stat(path)                           # yep, there!
#' read.table(gsutil_pipe(path, "r"))          # read from bucket
#' }
#' @export
avbucket <-
    function(namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        as_path = TRUE)
{
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(as_path)
    )

    .life_cycle(
        newfun = "avstorage",
        cycle = "deprecated",
        title = "av"
    )

    if (.avbucket_cache$exists(namespace, name)) {
        bucket <- .avbucket_cache$get(namespace, name)
    } else {
        response <- Terra()$getWorkspace(
            namespace, URLencode(name), "workspace.bucketName"
        )
        .avstop_for_status(response, "avbucket")
        bucket <- as.list(response)$workspace$bucketName
        .avbucket_cache$set(namespace, name, bucket)
    }

    if (as_path)
        bucket <- paste0("gs://", bucket)
    bucket
}

.avbucket_path <-
    function(bucket, ...)
{
    stopifnot(.gsutil_is_uri(bucket))

    ## get path without duplicate "/"
    args <- expand.grid(..., stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    args <- unname(as.list(args))
    bucket <- sub("/*$", "", bucket)
    args <- lapply(args, sub, pattern = "^/*", replacement = "")
    args <- lapply(args, sub, pattern = "/*$", replacement = "")
    args <- do.call(
        "mapply",
        c(list(
            FUN = paste,
            MoreArgs = list(sep = "/"),
            USE.NAMES = FALSE
        ), args)
    )
    paste0(bucket, ifelse(length(args), "/", ""), args)
}

#' @rdname av-deprecated
#'
#' @description `avfiles_ls()` returns the paths of files in the
#'     workspace bucket.  `avfiles_backup()` copies files from the
#'     compute node file system to the workspace bucket.
#'     `avfiles_restore()` copies files from the workspace bucket to
#'     the compute node file system.  `avfiles_rm()` removes files or
#'     directories from the workspace bucket.
#'
#' @param path For `avfiles_ls(), the character(1) file or directory
#'     path to list. For `avfiles_rm()`, the character() (perhaps with
#'     length greater than 1) of files or directory paths to be
#'     removed. The elements of `path` can contain glob-style
#'     patterns, e.g., `vign*`.
#'
#' @param full_names logical(1) return names relative to `path`
#'     (`FALSE`, default) or root of the workspace bucket?
#'
#' @param recursive logical(1) list files recursively?
#'
#' @param as_path logical(1) when TRUE (default) return bucket with
#'     prefix `gs://` (for `avbucket()`) or `gs://<bucket-id>` (for
#'     `avfiles_ls()`).
#'
#' @return `avfiles_ls()` returns a character vector of files in the
#'     workspace bucket.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     avfiles_ls()
#'
#' @export
avfiles_ls <-
    function(
        path = "",
        full_names = FALSE,
        recursive = FALSE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(path, zchar = TRUE),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .life_cycle(
        newfun = "avlist",
        cycle = "deprecated",
        title = "av"
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, path)
    result <- gsutil_ls(source, recursive = recursive)
    if (full_names) {
        sub(paste0(bucket, "/*"), "", result)
    } else {
        sub(paste0(source, "/*"), "", result)
    }
}

#' @rdname av-deprecated
#'
#' @details `avfiles_backup()` can be used to back-up individual files
#'     or entire directories, recursively.  When `recursive = FALSE`,
#'     files are backed up to the bucket with names approximately
#'     `paste0(destination, "/", basename(source))`.  When `recursive
#'     = TRUE` and source is a directory `path/to/foo/', files are
#'     backed up to bucket names that include the directory name,
#'     approximately `paste0(destination, "/", dir(basename(source),
#'     full.names = TRUE))`.  Naming conventions are described in
#'     detail in `gsutil_help("cp")`.
#'
#' @param source character() file paths. for `avfiles_backup()`,
#'     `source` can include directory names when `recursive = TRUE`.
#'
#' @param destination character(1) a google bucket
#'     (`gs://<bucket-id>/...`) to write files. The default is the
#'     workspace bucket.
#'
#' @param parallel logical(1) backup files using parallel transfer?
#'     See `?gsutil_cp()`.
#'
#' @return `avfiles_backup()` returns, invisibly, the status code of the
#'     `gsutil_cp()` command used to back up the files.
#'
#' @examples
#' \dontrun{
#' ## backup all files in the current directory
#' ## default buckets are gs://<bucket-id>/<file-names>
#' avfiles_backup(dir())
#' ## backup working directory, recursively
#' ## default buckets are gs://<bucket-id>/<basename(getwd())>/...
#' avfiles_backup(getwd(), recursive = TRUE)
#' }
#'
#' @export
avfiles_backup <-
    function(
        source,
        destination = "",
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        `some 'source' paths do not exist` = all(file.exists(source)),
        .is_scalar_character(destination, zchar = TRUE),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .life_cycle(
        newfun = "avbackup",
        cycle = "deprecated",
        title = "av"
    )

    bucket <- avbucket(namespace, name)
    destination <- .avbucket_path(bucket, destination)
    gsutil_cp(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av-deprecated
#'
#' @details `avfiles_restore()` behaves in a manner analogous to
#'     `avfiles_backup()`, copying files from the workspace bucket to
#'     the compute node file system.
#'
#' @export
avfiles_restore <-
    function(
        source,
        destination = ".",
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        .is_character(source),
        .is_scalar_character(destination),
        `'destination' is not a directory` = dir.exists(destination),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .life_cycle(
        newfun = "avrestore",
        cycle = "deprecated",
        title = "av"
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, source)
    gsutil_cp(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av-deprecated
#'
#' @return `avfiles_rm()` on success, returns a list of the return
#'     codes of `gsutil_rm()`, invisibly.
#'
#' @export
avfiles_rm <-
    function(
        source,
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        .is_character(source),
        .is_scalar_logical(recursive),
        .is_scalar_logical(parallel),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .life_cycle(
        newfun = "avremove",
        cycle = "deprecated",
        title = "av"
    )

    bucket <- avbucket(namespace, name)
    source <- .avbucket_path(bucket, source)
    result <- lapply(
        source, gsutil_rm, recursive = recursive, parallel = parallel
    )
    invisible(unlist(result))
}

.avnotebooks_runtime_path <-
    function(name)
{
    path.expand(file.path("~", name, "edit"))
}

.avnotebooks_workspace_path <-
    function(namespace, name)
{
    paste(avbucket(namespace, name), "notebooks", sep = "/")
}

#' @rdname av-deprecated
#'
#' @title Notebook management
#'
#' @description `avnotebooks()` returns the names of the notebooks
#'     associated with the current workspace.
#'
#' @param local = `logical(1)` notebooks located on the workspace
#'     (`local = FALSE`, default) or runtime / local instance (`local
#'     = TRUE`). When `local = TRUE`, the notebook path is
#'     `<avworkspace_name>/notebooks`.
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @return `avnotebooks()` returns a character vector of buckets /
#'     files located in the workspace 'Files/notebooks' bucket path,
#'     or on the local file system.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     avnotebooks()
#'
#' @export
avnotebooks <-
    function(local = FALSE,
             namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        .is_scalar_logical(local),
        !local || (.is_scalar_character(namespace) && .is_scalar_character(name))
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )

    if (local) {
        dir(.avnotebooks_runtime_path(name))
    } else {
        basename(gsutil_ls(.avnotebooks_workspace_path(namespace, name)))
    }
}

#' @rdname av-deprecated
#'
#' @description `avnotebooks_localize()` synchronizes the content of
#'     the workspace bucket to the local file system.
#'
#' @param destination missing or character(1) file path to the local
#'     file system directory for synchronization. The default location
#'     is `~/<avworkspace_name>/notebooks`. Out-of-date local files
#'     are replaced with the workspace version.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `avnotebooks_localize()` returns the exit status of
#'     `gsutil_rsync()`.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     avnotebooks_localize()  # dry run
#'
#' @export
avnotebooks_localize <-
    function(destination,
             namespace = avworkspace_namespace(), name = avworkspace_name(),
             dry = TRUE)
{
    ## FIXME: localize to persistent disk independent of current location
    ## .avnotebooks_localize_runtime(source, name, runtime_name, dry)

    stopifnot(
        missing(destination) || .is_scalar_character(destination),
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(dry)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )

    source <- .avnotebooks_workspace_path(namespace, name)
    if (missing(destination)) {
        destination = .avnotebooks_runtime_path(name)
        if (!dry && !dir.exists(destination))
            dir.create(destination, recursive = TRUE)
    }
    localize(source, destination, dry = dry)
}

#' @rdname av-deprecated
#'
#' @description `avnotebooks_delocalize()` synchronizes the content of
#'     the notebook location of the local file system to the workspace
#'     bucket.
#'
#' @param source missing or character(1) file path to the local file
#'     system directory for synchronization. The default location is
#'     `~/<avworkspace_name>/notebooks`. Out-of-date local files are
#'     replaced with the workspace version.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `avnotebooks_delocalize()` returns the exit status of
#'     `gsutil_rsync()`.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     try(avnotebooks_delocalize())  # dry run, fails if no local resource
#'
#' @export
avnotebooks_delocalize <-
    function(source,
             namespace = avworkspace_namespace(), name = avworkspace_name(),
             dry = TRUE)
{
    stopifnot(
        missing(source) || .is_scalar_character(source),
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(dry)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )

    if (missing(source))
        source <- .avnotebooks_runtime_path(name)
    destination <- .avnotebooks_workspace_path(namespace, name)
    delocalize(source, destination, dry = dry)
}

#' @rdname av-deprecated
#' @name avworkflow_configurations
#' @aliases avworkflow_configurations
#'
#' @title Workflow configuration
#'
#' @description Funtions on this help page facilitate getting,
#'     updating, and setting workflow configuration parameters. See
#'     `?avworkflow` for additional relevant functionality.
#'
#' @seealso The help page `?avworkflow` for discovering, running,
#'     stopping, and retrieving outputs from workflows.
NULL


##
## namespace / name management
##

.avworkflow <- local({
    hash <- new.env(parent = emptyenv())
    function(key, value) {
        if (!is.null(value))
            hash[[key]] <- value
        hash[[key]]
    }
})

#' @rdname av-deprecated
#'
#' @description `avworkflow_namespace()` and `avworkflow_name()` are
#'     utility functions to record the workflow namespace and name
#'     required when working with workflow
#'     configurations. `avworkflow()` provides a convenient way to
#'     provide workflow namespace and name in a single command,
#'     `namespace/name`.
#'
#' @param workflow_namespace character(1) AnVIL workflow namespace, as
#'     returned by, e.g., the `namespace` column of `avworkflows()`.
#'
#' @param workflow_name character(1) AnVIL workflow name, as returned
#'     by, e.g., the `name` column of `avworkflows()`.
#'
#' @param workflow character(1) representing the combined workflow
#'     namespace and name, as `namespace/name`.
#'
#' @return `avworkflow_namespace()`, and `avworkflow_name()` return
#'     `character(1)` identifiers. `avworkflow()` returns the
#'     character(1) concatenated namespace and name. The value
#'     returned by `avworkflow_name()` will be percent-encoded (e.g.,
#'     spaces `" "` replaced by `"%20"`).
#'
#' @export
avworkflow_namespace <-
    function(workflow_namespace = NULL)
{
    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )
    .avworkflow("NAMESPACE", workflow_namespace)
}

#' @rdname av-deprecated
#'
#' @export
avworkflow_name <-
    function(workflow_name = NULL)
{
    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )
    value <- .avworkflow("NAME", workflow_name)
    URLencode(value)
}

#' @rdname av-deprecated
#'
#' @export
avworkflow <-
    function(workflow = NULL)
{
    stopifnot(
        is.null(workflow) || .is_scalar_character(workflow)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )

    if (!is.null(workflow)) {
        wkflow <- strsplit(workflow, "/")[[1]]
        if (length(wkflow) != 2L)
            stop(
                "'workflow' must be fo the form 'namespace/name', ",
                "with a single '/'"
            )
        avworkflow_namespace(wkflow[[1]])
        avworkflow_name(wkflow[[2]])
    }
    paste0(avworkflow_namespace(), "/", avworkflow_name())
}

.avworkflow_response <-
    function(config)
{
    response <- Rawls()$method_inputs_outputs(
        config$name,
        config$namespace,
        config$methodRepoMethod$methodPath,
        config$methodRepoMethod$methodUri,
        config$methodRepoMethod$methodVersion,
        config$methodRepoMethod$sourceRepo
    )
    .avstop_for_status(response, "avworkflow_response")
}

#' @importFrom dplyr left_join
.avworkflow_configuration_template_io <-
    function(template, config)
{
    if (length(config)) {
        config <-
            tibble::enframe(config, value = "attribute") |>
            tidyr::unnest("attribute")
        template <- left_join(
            template,
            config,
            by = "name"
        )
    } else {
        template <- cbind(template, attribute = character(nrow(template)))
    }
    as_tibble(template)
}

#' @rdname av-deprecated
#'
#' @description `avworkflow_configuration_get()` returns a list structure
#'     describing an existing workflow configuration.
#'
#' @return `avworkflow_configuration_get()` returns a list structure
#'     describing the configuration. See
#'     `avworkflow_configuration_template()` for the structure of a
#'     typical workflow.
#'
#' @export
avworkflow_configuration_get <-
    function(workflow_namespace = avworkflow_namespace(),
             workflow_name = avworkflow_name(),
             namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(workflow_name),
        .is_scalar_character(workflow_namespace),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )

    config <- Rawls()$get_method_configuration(
        namespace, URLencode(name),
        workflow_namespace, workflow_name
    )
    .avstop_for_status(config, "avworkflow_methods")
    config_list <- config %>% as.list()
    class(config_list) <- c("avworkflow_configuration", class(config_list))
    config_list
}

#' @rdname av-deprecated
#'
#' @description `avworkflow_configuration_inputs()` returns a
#'     data.frame template for the inputs defined in a workflow
#'     configuration.  This template can be used to provide custom
#'     inputs for a configuration.
#'
#' @return `avworkflow_configuration_inputs()` returns a data.frame
#'     providing a template for the configuration inputs, with the
#'     following columns:
#'
#' - inputType
#' - name
#' - optional
#' - attribute
#'
#' @return The only column of interest to the user is the `attribute`
#'     column, this is the column that should be changed for
#'     customization.
#'
#' @export
avworkflow_configuration_inputs <-
    function(config)
{
    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )
    stopifnot(inherits(config, "avworkflow_configuration"))
    response <- .avworkflow_response(config)
    inputs_tmpl <- as.list(response)$inputs
    inputs_config <- config$inputs
    .avworkflow_configuration_template_io(inputs_tmpl, inputs_config)
}

#' @rdname av-deprecated
#'
#' @description `avworkflow_configuration_outputs()` returns a
#'     data.frame template for the outputs defined in a workflow
#'     configuration.  This template can be used to provide custom
#'     outputs for a configuration.
#'
#' @return `avworkflow_configuration_outputs()` returns a data.frame
#'     providing a template for the configuration outputs, with the
#'     following columns:
#' - name
#' - outputType
#' - attribute
#'
#' @return The only column of interest to the user is the `attribute`
#'     column, this is the column that should be changed for
#'     customization.
#'
#' @export
avworkflow_configuration_outputs <-
    function(config)
{
    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )
    stopifnot(inherits(config, "avworkflow_configuration"))
    response <- .avworkflow_response(config)
    outputs_tmpl <- as.list(response)$outputs
    outputs_config <- config$outputs
    .avworkflow_configuration_template_io(outputs_tmpl, outputs_config)
}

.avworkflow_MethodRepoMethod_validate <-
    function(methodRepoMethod, .schema)
{
    args <- formals(.schema)
    unknown <- setdiff(names(methodRepoMethod), names(args))
    if (length(unknown))
        stop(
            "unknown 'methodRepoMethod' names: '",
            paste(unknown, collapse = "' '"),
            "'"
        )
    idx <- !names(methodRepoMethod) %in% "methodVersion"
    ok <- vapply(methodRepoMethod[idx], .is_scalar_character, logical(1))
    if (!all(ok))
        stop(
            "'methodRepoMethod' values must be character(1); bad values: '",
            paste(names(ok)[!ok], collapse = "' '"),
            "'"
        )
    methodVersion <- methodRepoMethod$methodVersion
    stopifnot(
        `'methodRepoMethod$methodVersion' must be character(1) or integer(1)` =
            .is_scalar_character(methodVersion) ||
            .is_scalar_integer(as.integer(methodVersion))
    )

    ## all elements are unboxed
    lapply(methodRepoMethod, jsonlite::unbox)
}

.avworkflow_configuration_set_validate_response <-
    function(response, op)
{
    lens <- lengths(response)
    bad <- grepl("^(extra|invalid)", names(lens)) & lens != 0L
    if (any(bad)) {
        warning(
            "'avworkflow_configuration_set' bad values:", immediate. = TRUE
        )
        sink(stderr())
        on.exit(sink(NULL))
        print(response[bad])
    }

    invisible(response)
}

#' @rdname av-deprecated
#'
#' @description `avworkflow_configuration_update()` returns a list structure
#'     describing a workflow configuration with updated inputs and / or outputs.
#'
#' @param config an `avworkflow_configuration` object to be updated.
#'
#' @param inputs the new inputs to be updated in the workflow configuration. If
#'     none are specified, the inputs from the original configuration will be
#'     used and no changes will be made.
#'
#' @param outputs the new outputs to be updated in the workflow configuration.
#'     If none are specified, the outputs from the original configuration will
#'     be used and no changes will be made.
#'
#' @return `avworkflow_configuration_update()` returns a list structure
#'     describing the updated configuration.
#'
#' @export
avworkflow_configuration_update <-
    function(config,
             inputs = avworkflow_configuration_inputs(config),
             outputs = avworkflow_configuration_outputs(config))
{
    stopifnot(
        inherits(config, "avworkflow_configuration"),
        all(c("name", "attribute") %in% names(inputs)),
        all(c("name", "attribute") %in% names(outputs)),
        is(inputs, "data.frame"), is(outputs, "data.frame")
    )
    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )
    config$inputs <-
        lapply(setNames(as.list(inputs$attribute), inputs$name), unbox)
    config$outputs <-
        lapply(setNames(as.list(outputs$attribute), outputs$name), unbox)
    config
}

#' @rdname av-deprecated
#'
#' @description `avworkflow_configuration_set()` updates an existing
#'     configuration in Terra / AnVIL, e.g., changing inputs to the
#'     workflow.
#'
#' @param config a named list describing the full configuration, e.g.,
#'     created from editing the return value of
#'     `avworkflow_configuration_set()` or
#'     `avworkflow_configuration_template()`.
#'
#' @param dry logical(1) when `TRUE` (default), report the
#'     consequences but do not perform the action requested. When
#'     `FALSE`, perform the action.
#'
#' @return `avworkflow_configuration_set()` returns an object
#'     describing the updated configuration. The return value includes
#'     invalid or unused elements of the `config` input. Invalid or
#'     unused elements of `config` are also reported as a warning.
#'
#' @examples
#' ## set the namespace and name as appropriate
#' avworkspace("bioconductor-rpci-anvil/Bioconductor-Workflow-DESeq2")
#'
#' ## discover available workflows in the workspace
#' if (gcloud_exists())
#'     avworkflows()
#'
#' ## record the workflow of interest
#' avworkflow("bioconductor-rpci-anvil/AnVILBulkRNASeq")
#'
#' ## what workflows are available?
#' if (gcloud_exists()) {
#'     available_workflows <- avworkflows()
#'
#'     ## retrieve the current configuration
#'     config <- avworkflow_configuration_get()
#'     config
#'
#'     ## what are the inputs and outputs?
#'     inputs <- avworkflow_configuration_inputs(config)
#'     inputs
#'
#'     outputs <- avworkflow_configuration_outputs(config)
#'     outputs
#'
#'     ## update inputs or outputs, e.g., this input can be anything...
#'     inputs <-
#'         inputs |>
#'         mutate(attribute = ifelse(
#'             name == "salmon.transcriptome_index_name",
#'             '"new_index_name"',
#'             attribute
#'         ))
#'     new_config <- avworkflow_configuration_update(config, inputs)
#'     new_config
#'
#'     ## set the new configuration in AnVIL; use dry = FALSE to actually
#'     ## update the configuration
#'     avworkflow_configuration_set(config)
#' }
#'
#' ## avworkflow_configuration_template() is a utility function that may
#' ## help understanding what the inputs and outputs should be
#' avworkflow_configuration_template() |>
#'     str()
#'
#' @export
avworkflow_configuration_set <-
    function(config,
             namespace = avworkspace_namespace(),
             name = avworkspace_name(),
             dry = TRUE)
{
    stopifnot(
        inherits(config, "avworkflow_configuration"),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )

    rawls <- Rawls()

    config$methodRepoMethod <- .avworkflow_MethodRepoMethod_validate(
        config$methodRepoMethod, schemas(rawls)$MethodRepoMethod
    )

    if (dry) {
        message(
            "'avworkflow_configuration_set()' arguments validated; ",
            "use 'dry = FALSE' to update ",
            "workflow ", paste0(config$namespace, "/", config$name), " in ",
            "workspace ", paste0(namespace, "/", name)
        )
        return(invisible(config))
    }

    response <- rawls$update_method_configuration(
        namespace, URLencode(name), config$namespace, config$name,
        .__body__ = config
    )
    .avstop_for_status(response, "avworkflow_configuration_set")

    response <-
        response %>%
        as.list()
    .avworkflow_configuration_set_validate_response(response)
    return(invisible(config))
}

#' @rdname av-deprecated
#'
#' @description `avworkflow_configuration_template()` returns a
#'     template for defining workflow configurations. This template
#'     can be used as a starting point for providing a custom
#'     configuration.
#'
#' @details The exact format of the configuration is important.
#'
#' One common problem is that a scalar character vector `"bar"` is
#' interpretted as a json 'array' `["bar"]` rather than a json string
#' `"bar"`. Enclose the string with `jsonlite::unbox("bar")` in the
#' configuration list if the length 1 character vector in R is to be
#' interpretted as a json string.
#'
#' A second problem is that an unquoted unboxed character string
#' `unbox("foo")` is required by AnVIL to be quoted. This is reported
#' as a warning() about invalid inputs or outputs, and the solution is
#' to provide a quoted string `unbox('"foo"')`.
#'
#' @return `avworkflow_configuration_template()` returns a list
#'     providing a template for configuration lists, with the
#'     following structure:
#'
#' - namespace character(1) configuration namespace.
#' - name character(1) configuration name.
#' - rootEntityType character(1) or missing. the name of the table
#'   (from `avtables()`) containing the entitites referenced in
#'   inputs, etc., by the keyword 'this.'
#' - prerequisites named list (possibly empty) of prerequisites.
#' - inputs named list (possibly empty) of inputs. Form of input
#'   depends on method, and might include, e.g., a reference to a
#'   field in a table referenced by `avtables()` or a character string
#'   defining an input constant.
#' - outputs named list (possibly empty) of outputs.
#' - methodConfigVersion integer(1) identifier for the method
#'   configuration.
#' - methodRepoMethod named list describing the method, with
#'   character(1) elements described in the return value for `avworkflows()`.
#'   - methodUri
#'   - sourceRepo
#'   - methodPath
#'   - methodVersion. The REST specification indicates that this has
#'     type `integer`, but the documentation indicates either
#'     `integer` or `string`.
#' - deleted logical(1) of uncertain purpose.
#'
#' @examples
#' avworkflow_configuration_template()
#'
#' @export
avworkflow_configuration_template <-
    function()
{
    .life_cycle(
        newpackage = "AnVILGCP",
        cycle = "deprecated",
        title = "av"
    )
    list(
        ## method namespace and name, not workspace namespace and name
        namespace = character(1),
        name = character(1),
        rootEntityType = character(0),
        prerequisites = setNames(list(), character()),
        inputs = setNames(list(), character()),
        outputs = setNames(list(), character()),
        methodConfigVersion = integer(1),
        methodRepoMethod = list(
            methodUri = unbox(character(1)),
            sourceRepo = unbox(character(1)),
            methodPath = unbox(character(1)),
            methodVersion = unbox(character(1))
        ),
        deleted = logical(1)
    )
}

#' @rdname av-deprecated
#'
#' @param x Object of class `avworkflow_configuration`.
#'
#' @param ... additional arguments to `print()`; unused.
#'
#' @export
print.avworkflow_configuration <-
    function(x, ...)
{
    str(x)
}

#' @rdname av-deprecated
#'
#' @aliases avworkflow_configuration
#'
#' @title Deprecated functions in package \sQuote{AnVIL}
#'
#' @description These functions are provided for compatibility with
#'     older versions of \sQuote{AnVIL} only, and will be defunct at
#'     the next release.
#'
#' @param configuration_namespace character(1).
#'
#' @param configuration_name character(1).
#'
#' @param config `avworkflow_configuration` object.
#'
#' @param namespace character(1).
#'
#' @param name character(1).
#'
#' @details The following functions are deprecated and will be made
#'     defunct; use the replacement indicated below:
#'
#' - `avworkflow_configuration()`: \code{\link{avworkflow_configuration_get}}
#' - `avworkflow_import_configuration()`: \code{\link{avworkflow_configuration_set}}
avworkflow_configuration <-
    function(configuration_namespace, configuration_name,
             namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    .life_cycle(
        newfun = "avworkflow_configuration_get",
        cycle = "deprecated",
        title = "av"
    )
    avworkflow_configuration_get(
        configuration_namespace, configuration_name,
        namespace, name
    )
}

#' @rdname av-deprecated
avworkflow_import_configuration <-
    function(config,
             namespace = avworkspace_namespace(), name = avworkspace_name())
{
    .life_cycle(
        newfun = "avworkflow_configuration_set",
        cycle = "deprecated",
        title = "av"
    )
    avworkflow_configuration_set(config, namespace = namespace, name = name)
}
