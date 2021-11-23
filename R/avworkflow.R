#' @rdname avworkflow
#' @md
#'
#' @title Workflow submissions and file outputs
#'
#' @inheritParams avworkspace
#'
#' @description `avworkflows()` returns a tibble summarizing available
#'     workflows.
#'
#' @return `avworkflows()` returns a tibble. Each workflow is in a
#'     'namespace' and has a 'name', as illustrated in the
#'     example. Columns are
#'
#' - name: workflow name.
#' - namespace: workflow namespace (often the same as the workspace namespace).
#' - rootEntityType: name of the `avtable()` used to retrieve inputs.
#' - methodRepoMethod.methodUri: source of the method, e.g., a dockstore URI.
#' - methodRepoMethod.sourceRepo: source repository, e.g., dockstore.
#' - methodRepoMethod.methodPath: path to method, e.g., a dockerstore
#'   method might reference a github repository.
#' - methodRepoMethod.methodVersion: the version of the method, e.g.,
#'   'main' branch of a github repository.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     ## from within AnVIL
#'     avworkflows() %>% select(namespace, name)
#'
#' @export
avworkflows <-
    function(namespace = avworkspace_namespace(), name = avworkspace_name())
{
    workflows <- Rawls()$list_method_configurations(namespace, name, TRUE)
    .avstop_for_status(workflows, "avworkflows")
    workflows %>% flatten()
}

.avworkflow_job <-
    function(x)
{
    succeeded <- 0L
    failed <- 0L
    if ("Succeeded" %in% names(x$workflowStatuses))
        succeeded <- x$workflowStatuses$Succeeded
    if ("Failed" %in% names(x$workflowStatuses))
        failed <- x$workflowStatuses$Failed

    list(
      submissionId = x[["submissionId"]],
      submitter = x[["submitter"]],
      submissionDate = x[["submissionDate"]],
      status = x[["status"]],
      succeeded = succeeded,
      failed = failed
    )
}

#' @rdname avworkflow
#' @md
#'
#' @description `avworkflow_jobs()` returns a tibble summarizing
#'     submitted workflow jobs for a namespace and name.
#'
#' @return `avworkflow_jobs()` returns a tibble, sorted by
#'     `submissionDate`, with columns
#'
#' - submissionId character() job identifier from the workflow runner.
#' - submitter character() AnVIL user id of individual submitting the job.
#' - submissionDate POSIXct() date (in local time zone) of job submission.
#' - status character() job status, with values 'Accepted' 'Evaluating'
#'   'Submitting' 'Submitted' 'Aborting' 'Aborted' 'Done'
#' - succeeded integer() number of workflows succeeding.
#' - failed integer() number of workflows failing.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     ## from within AnVIL
#'     avworkflow_jobs()
#'
#' @importFrom dplyr bind_rows mutate desc
#'
#' @export
avworkflow_jobs <-
    function(namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )
    terra <- Terra()
    response <- terra$listSubmissions(namespace, name)
    .avstop_for_status(response, "avworkflow_jobs")

    submissions <- content(response, encoding = "UTF-8")
    if (length(submissions)) {
        submissions <- lapply(submissions, .avworkflow_job)
    } else {
        submissions <- list(
            submissionId = character(),
            submitter = character(),
            submissionDate = character(),
            status = character(),
            succeeded = integer(),
            failed = integer()
        )
    }
    bind_rows(submissions) %>%
        mutate(
            submissionDate =
                .POSIXct(as.numeric(
                    as.POSIXct(.data$submissionDate, "%FT%T", tz="UTC")
                ))
        ) %>%
        arrange(desc(.data$submissionDate))
}

.WORKFLOW_LOGS <- "workflow.logs"

.WORKFLOW_CONTROL_FILES <- c(
    "gcs_delocalization\\.sh",
    "gcs_localization\\.sh",
    "gcs_transfer\\.sh",
    "output", "rc", "script",
    "stderr", "stdout",
    "workflow\\..*\\.log"
    ## also task-specific logs
    ## "hmmratac_run\\.log",
)

.avworkflow_file_type <-
    function(file, workflow)
{
    type <- rep("output", length(file))
    fls <- c(
        .WORKFLOW_CONTROL_FILES,
        paste0(unique(workflow), "_run-*[[:digit:]]*\\.log")
    )
    pattern <- paste0("^(", paste(fls, collapse = "|"), ")$")
    type[grepl(pattern, file)] <- "control"
    type
}

#' @rdname avworkflow
#' @md
#'
#' @description `avworkflow_files()` returns a tibble containing
#'     information and file paths to workflow outputs.
#'
#' @param submissionId a character() of workflow submission ids, or a
#'     tibble with column `submissionId`, or NULL / missing. See
#'     'Details'.
#'
#' @param bucket character(1) name of the google bucket in which the
#'     workflow products are available, as `gs://...`. Usually the
#'     bucket of the active workspace, returned by `avbucket()`.
#'
#' @details For `avworkflow_files()`, the `submissionId` is the
#'     identifier associated with the workflow job, and is present in
#'     the return value of `avworkflow_jobs()`; the example
#'     illustrates how the first row of `avworkflow_jobs()` (i.e., the
#'     most recenltly completed workflow) can be used as input to
#'     `avworkflow_files()`. When `submissionId` is not provided, the
#'     return value is for the most recently submitted workflow of the
#'     namespace and name of `avworkspace()`.
#'
#' @return `avworkflow_files()` returns a tibble with columns
#'
#' - file: character() 'base name' of the file in the bucket.
#' - workflow: character() name of the workflow the file is associated
#'   with.
#' - task: character() name of the task in the workflow that generated
#'   the file.
#' - path: charcter() full path to the file in the google bucket.
#'
#' @importFrom tibble is_tibble
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#'     ## e.g., from within AnVIL
#'     avworkflow_jobs() %>%
#'     ## select most recent workflow
#'     head(1) %>%
#'     ## find paths to output and log files on the bucket
#'     avworkflow_files()
#' }
#'
#' @export
avworkflow_files <-
    function(submissionId = NULL, bucket = avbucket())
{
    stopifnot(
        .is_scalar_character(bucket),
        is.null(submissionId) || .is_character(submissionId) ||
        (is_tibble(submissionId) && "submissionId" %in% names(submissionId))
    )

    if (is.null(submissionId))
        ## default: most recent workflow job
        submissionId <-
            avworkflow_jobs() %>%
            head(1)

    if (is_tibble(submissionId))
        submissionId <- submissionId$submissionId

    if (length(submissionId)) {
        path0 <- paste0(bucket, "/", submissionId)
        path <- gsutil_ls(path0, recursive = TRUE)
    } else {
        path <- character()
    }

    part <- strsplit(path, "/")
    workflow <- vapply(part, `[[`, character(1), 5)
    task <- rep(NA_character_, length(workflow))
    idx <- workflow != .WORKFLOW_LOGS
    task[idx] <- vapply(part[idx], `[[`, character(1), 7)
    tbl <-  tibble(
        file = basename(path),
        workflow = workflow,
        task = task,
        type = .avworkflow_file_type(file, workflow),
        path = path
    )
    tbl %>%
        arrange(
            match(.data$type, c("output", "control")), # output first
            task, file, path
        )
}

#' @rdname avworkflow
#' @md
#'
#' @description `avworkflow_localize()` creates or synchronizes a
#'     local copy of files with files stored in the workspace bucket
#'     and produced by the workflow.
#'
#' @details `avworkflow_localize()`. `type = "control"` files
#'     summarize workflow progress; they can be numerous but are
#'     frequently small and quickly syncronized. `type = "output"`
#'     files are the output products of the workflow stored in the
#'     workspace bucket. Depending on the workflow, outputs may be
#'     large, e.g., aligned reads in bam files. See `gsutil_cp()` to
#'     copy individual files from the bucket to the local drive.
#'
#'     `avworkflow_localize()` treats `submissionId=` in the same way
#'     as `avworkflow_files()`: when missing, files from the most
#'     recent workflow job are candidates for localization.
#'
#' @param destination character(1) file path to the location where
#'     files will be synchronized. For directories in the current
#'     working directory, be sure to prepend with `"./"`. When `NULL`,
#'     the `submissionId` is used as the destination. `destination`
#'     may also be a google bucket, in which case th workflow files
#'     are synchronized from the workspace to a second bucket.
#'
#' @param type character(1) copy `"control"` (default), `"output"`, or
#'     `"all"` files produced by a workflow.
#'
#' @param dry logical(1) when `TRUE` (default), report the
#'     consequences but do not perform the action requested. When
#'     `FALSE`, perform the action.
#'
#' @return `avworkflow_localize()` prints a message indicating the
#'     number of files that are (if `dry = FALSE`) or would be
#'     localized. If no files require localization (i.e., local files
#'     are not older than the bucket files), then no files are
#'     localized. `avworkflow_localize()` returns a tibble of file
#'     name and bucket path of files to be synchronized.
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#'     avworkflow_localize(dry = TRUE)
#' }
#'
#' @export
avworkflow_localize <-
    function(
         submissionId = NULL,
         destination = NULL,
         type = c("control", "output", "all"),
         bucket = avbucket(),
         dry = TRUE
    )
{
    type <- match.arg(type)
    stopifnot(
        .is_scalar_logical(dry),
        is.null(destination) || .is_scalar_character(destination)
    )
    if (is.null(submissionId))
        submissionId <-
            avworkflow_jobs() %>%
            pull(submissionId) %>%
            head(1)
    if (is.null(destination))
        destination <- paste0("./", submissionId)
    if (dry && !dir.exists(destination)) {
        ## create temporary 'destination' so gsutil_rsync does not fail
        destination <- tempfile()
        dir.create(destination)
    }

    fls <- avworkflow_files(submissionId, bucket)

    source <- paste(avbucket(), submissionId, sep = "/")
    exclude <- NULL
    exclude0 <- unique(fls$file[!fls$type %in% type])
    exclude1 <- gsub(".", "\\.", paste0(exclude0, collapse = "|"), fixed = TRUE)
    if (nzchar(exclude1))
        exclude <- paste0(".*/(", exclude1, ")$")

    result <- gsutil_rsync(
        source, destination, delete = FALSE,
        recursive = TRUE, exclude = exclude, dry = dry
    )
    if (dry) {
        idx <- startsWith(result, "Would copy ")
        result <- sub("Would copy (.*) to .*", "\\1", result[idx])
        n_files <- length(result)
        warning(
            "use 'dry = FALSE' to localize ", n_files, " workflow files",
            call. = FALSE
        )
    } else {
        idx <- startsWith(result, "Copying ")
        result <- sub("Copying (.*)\\.\\.\\.", "\\1", result[idx])
        n_files <- length(result)
        message("localized ", n_files, " workflow files to '", destination, "'")
    }

    tbl <- tibble(
        file = basename(result),
        path = result
    )

    invisible(tbl)
}

#' @rdname avworkflow
#' @md
#'
#' @description `avworkflow_configuration_template()` returns a
#'     template for defining workflow configurations. This template
#'     can be used as a starting point for providing a custom
#'     configuration.
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
#' @return The exact format of the configuration is important.
#'
#'     One common problem is that a scalar character vector `"bar"` is
#'     interpretted as a json 'array' `["bar"]` rather than a json
#'     string `"bar"`. Enclose the string with
#'     `jsonlite::unbox("bar")` in the configuration list if the
#'     length 1 character vector in R is to be interpretted as a json
#'     string.
#'
#'     A second problem is that an unquoted unboxed character string
#'     `unbox("foo")` is required by AnVIL to be quoted. This is
#'     reported as a warning() about invalid inputs or outputs, and
#'     the solution is to provide a quoted string `unbox('"foo"')`.
#'
#' @examples
#' avworkflow_configuration_template()
#'
#' @export

avworkflow_configuration_template <-
    function()
{
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

.response <-
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
    .avstop_for_status(response)
}

avworkflow_configuration_inputs_template <-
    function(config)
{
    response <- .response(config)
    inputs_tmpl <- as.list(response)$inputs
    if (length(config$inputs)) {
        inputs_config <-
            tibble::enframe(config$inputs, value = "attribute") |>
            tidyr::unnest(attribute)
        inputs_tmpl <- left_join(
            inputs_tmpl,
            inputs_config,
            by = "name"
        )
    } else {
        inputs_tmpl <- cbind(inputs_tmpl, attribute = character(nrow(inputs_tmpl)))
    }
    inputs_tmpl
}

avworkflow_configuration_outputs_template <-
    function(config)
{
    response <- .response(config)
    outputs_tmpl <- as.list(response)$outputs
    if (length(config$outputs)) {
        outputs_config <-
            tibble::enframe(config$outputs, value = "attribute") |>
            tidyr::unnest(attribute)
        outputs_tmpl <- left_join(
            outputs_tmpl,
            outputs_config,
            by = "name"
        )
    } else {
        outputs_tmpl <- cbind(outputs_tmpl, attribute = character(nrow(outputs_tmpl)))
    }
    outputs_tmpl
}

#' @rdname avworkflow
#' @md
#'
#' @description Produce print method for class `avworkflow_configuration`.
#'
#' @param x Object of class `avworkflow_configuration`.
#'
#' @return none
#'
#' @examples
#' \dontrun{
#' config <-
#'     avworkflow_configuration("bioconductor-anvil-rpci", "AnVILBulkRNASeq")
#' }
#'
#' @export
print.avworkflow_configuration <- function(x, ...) str(x)

#' @rdname avworkflow
#' @md
#'
#' @description `avworkflow_configuration()` returns a list structure
#'     describing an existing workflow configuration.
#'
#' @param configuration_namespace character(1) namespace of the
#'     workflow. Often the same as the namespace of the
#'     workspace. Discover configuration namespace and name
#'     information from `avworkflows()`.
#'
#' @param configuration_name character(1) name of the workflow, from
#'     `avworkflows()`
#'
#' @return `avworkflow_configuration()` returns a list structure
#'     describing the configuration. See
#'     `avworkflow_configuration_template()` for the structure of a
#'     typical workflow.
#'
#' @examples
#' \dontrun{
#' config <-
#'     avworkflow_configuration("bioconductor-anvil-rpci", "AnVILBulkRNASeq")
#' }
#'
#' @export
avworkflow_configuration <-
    function(configuration_namespace, configuration_name,
             namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    config <- Rawls()$get_method_configuration(
        namespace, name,
        configuration_namespace, configuration_name
    )
    .avstop_for_status(config, "avworkflow_methods")
    config_list <- config %>% as.list()
    class(config_list) <- c("avworkflow_configuration", class(config_list))
    config_list
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

.avworkflow_import_configuration_validate_response <-
    function(response, op)
{
    lens <- lengths(response)
    bad <- grepl("^(extra|invalid)", names(lens)) & lens != 0L
    if (any(bad)) {
        warning(
            "'avworkflow_import_configuration' bad values:", immediate. = TRUE
        )
        sink(stderr())
        on.exit(sink(NULL))
        print(response[bad])
    }

    invisible(response)
}

avworkflow_update_configuration_inputs <-
    function(config, inputs_template)
{
    config$inputs <-
        setNames(as.list(inputs_template$attribute), inputs_template$name)
}

avworkflow_update_configuration_outputs <-
    function(config, outputs_template)
{
    config$outputs <-
        setNames(as.list(outputs_template$attribute), outputs_template$name)
}

#' @rdname avworkflow
#' @md
#'
#' @description `avworkflow_import_configuration()` updates an
#'     existing configuration, e.g., changing inputs to the workflow.
#'
#' @param config a named list describing the full configuration, e.g.,
#'     created from editing the return value of
#'     `avworkflow_configuration()` or
#'     `avworkflow_configuration_template()`.
#'
#' @return `avworkflow_import_configuration()` returns an object
#'     describing the updated configuration. The return value includes
#'     invalid or unused elements of the `config` input. Invalid or
#'     unused elements of `config` are also reported as a warning.
#'
#' @examples
#' \dontrun{
#' avworkflow_import_configuration(config)
#' }
#'
#' @export
avworkflow_import_configuration <-
    function(config,
             namespace = avworkspace_namespace(), name = avworkspace_name())
{
    rawls <- Rawls()

    config$methodRepoMethod <- .avworkflow_MethodRepoMethod_validate(
        config$methodRepoMethod, schemas(rawls)$MethodRepoMethod
    )

    response <- rawls$update_method_configuration(
        namespace, name, config$namespace, config$name,
        .__body__ = config
    )
    .avstop_for_status(response, "avworkflow_import_configuration")

    response <-
        response %>%
        as.list()
    .avworkflow_import_configuration_validate_response(response)
}
