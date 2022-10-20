#' @rdname avworkflow
#'
#' @name avworkflows
#'
#' @title Workflow submissions and file outputs
#'
#' @inheritParams avworkspace
NULL

#' @rdname avworkflow
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
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )
    workflows <- Rawls()$list_method_configurations(
        namespace, URLencode(name), TRUE
    )
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

    response <- Terra()$listSubmissions(namespace, URLencode(name))
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
        bucket_content <- gsutil_ls(bucket)
        objects <- sub(paste0(bucket, "/([^/]+).*"), "\\1", bucket_content)
        idx <- submissionId %in% objects
        path0 <- paste0(bucket, "/", submissionId[idx])
        if (any(idx)) {
            path <- gsutil_ls(path0, recursive = TRUE)
        } else {
            path <- character()
        }
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
#' `avworkflow_localize()` treats `submissionId=` in the same way as
#' `avworkflow_files()`: when missing, files from the most recent
#' workflow job are candidates for localization.
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
    if (is.null(submissionId))
        submissionId <-
            avworkflow_jobs() %>%
            pull(submissionId) %>%
            head(1)

    stopifnot(
        .is_scalar_logical(dry),
        is.null(destination) || .is_scalar_character(destination),
        .is_scalar_character(submissionId)
    )

    if (is.null(destination))
        destination <- paste0("./", submissionId)
    if (dry && !dir.exists(destination)) {
        ## create temporary 'destination' so gsutil_rsync does not fail
        destination <- tempfile()
        dir.create(destination)
    }

    fls <- avworkflow_files(submissionId, bucket)
    objects <- sub(paste0(bucket, "/([^/]+).*"), "\\1", fls$path)
    if (!submissionId %in% objects) {
        message(
            "'avworkflow_localize()' found no objects for submissionId ",
            submissionId
        )
        return(invisible(tibble(file = character(), path = character())))
    }

    source <- paste(bucket, submissionId, sep = "/")
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
        message(
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
#'
#' @description `avworkflow_run()` runs the workflow of the configuration.
#'
#' @param config a `avworkflow_configuration` object of the workflow that will
#'     be run.
#'
#' @param entityName character(1) or NULL name of the set of samples
#'     to be used when running the workflow. NULL indicates that no
#'     sample set will be used.
#'
#' @param entityType character(1) or NULL type of root entity used for
#'     the workflow. NULL means that no root entity will be used.
#'
#' @param deleteIntermediateOutputFiles logical(1) whether or not to delete
#'     intermediate output files when the workflow completes.
#'
#' @param useCallCache logical(1) whether or not to read from cache for this
#'     submission.
#'
#' @param useReferenceDisks logical(1) whether or not to use pre-built
#'     disks for common genome references. Default: `FALSE`.
#'
#' @return `avworkflow_run()` returns `config`, invisibly.
#'
#' @examples
#' \dontrun{
#' entityName <- avtable("participant_set") |>
#'     pull(participant_set_id) |>
#'     head(1)
#' avworkflow_run(new_config, entityName)
#' }
#'
#' @export
avworkflow_run <-
    function(config,
        entityName,
        entityType = config$rootEntityType,
        deleteIntermediateOutputFiles = FALSE, useCallCache = TRUE,
        useReferenceDisks = FALSE,
        namespace = avworkspace_namespace(), name = avworkspace_name(),
        dry = TRUE)
{
    stopifnot(
        inherits(config, "avworkflow_configuration"),
        .is_scalar_character_or_NULL(entityName),
        .is_scalar_character_or_NULL(entityType),
        .is_scalar_logical(deleteIntermediateOutputFiles),
        .is_scalar_logical(useCallCache),
        .is_scalar_logical(useReferenceDisks),
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(dry)
    )

    if (dry) {
        message(
            "'avworkflow_run()' arguments validated, use 'dry = FALSE' ",
            "to run the workflow"
        )
        return(invisible(config))
    }

    run_workflow <- Rawls()$createSubmission(
        workspaceNamespace = namespace,
        workspaceName = URLencode(name),
        deleteIntermediateOutputFiles = deleteIntermediateOutputFiles,
        entityName = entityName,
        entityType = entityType,
        ## expression = "this"
        ## memoryRetryMultiplie
        methodConfigurationName = config$name,
        methodConfigurationNamespace = config$namespace,
        useCallCache = useCallCache,
        useReferenceDisks = useReferenceDisks,
        ## userComment                  : NULL
        workflowFailureMode = "NoNewCalls")

    .avstop_for_status(run_workflow, "avworkflow_run")

    invisible(config)
}

#' @rdname avworkflow
#'
#' @description `avworkflow_stop()` stops the most recently submitted workflow
#'     jub from running.
#'
#' @return `avworkflow_stop()` returns (invisibly) `TRUE` on
#'     successfully requesting that the workflow stop, `FALSE` if the
#'     workflow is already aborting, aborted, or done.
#'
#' @examples
#' \dontrun{
#' avworkflow_stop()
#' }
#'
#' @export
avworkflow_stop <-
    function(submissionId = NULL,
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        dry = TRUE)
{
    if (is.null(submissionId)) {
        submissionId <- avworkflow_jobs() |>
            pull(submissionId) |>
            head(1)
    }

    stopifnot(
        .is_scalar_character(submissionId),
        .is_scalar_character(namespace),
        .is_scalar_character(name),
        .is_scalar_logical(dry)
    )

    if (dry) {
        message(
            .pretty_text(
                "'avworkflow_stop()' arguments validated, use 'dry = FALSE'",
                "to stop the submission"
            ), "\n",
            "  namespace: ", namespace, "\n",
            "  name: ", name, "\n",
            "  submissionId: ", submissionId, "\n"
        )
        return(invisible(FALSE))
    }

    terra <- Terra()

    ## only change status of submitted / running workflows. In
    ## particular do not change the status of 'Done' workflows to
    ## 'Aborted'. https://github.com/Bioconductor/AnVIL/issues/64
    response <- terra$monitorSubmission(
        workspaceNamespace = namespace,
        workspaceName = URLencode(name),
        submissionId = submissionId)
    .avstop_for_status(response, "avworkflow_stop (current status)")
    current_status <- content(response, encoding = "UTF-8")$status
    WORKFLOW_STATUS_ENUM <- c("Aborting", "Aborted", "Done")
    if (current_status %in% WORKFLOW_STATUS_ENUM) {
        message(
            .pretty_text(
                "'avworkflow_stop()' will not change the status of workflows",
                "that are already aborting, aborted, or done"
            ), "\n",
            "  namespace: ", namespace, "\n",
            "  name: ", name, "\n",
            "  submissionId: ", submissionId, "\n",
            "  current status: ", current_status
        )
        return(invisible(FALSE))
    }

    if (dry) {
        message(
            .pretty_text(
                "'avworkflow_stop()' arguments validated, use 'dry = FALSE'",
                "to stop the submission"
            ), "\n",
            "  namespace: ", namespace, "\n",
            "  name: ", name, "\n",
            "  submissionId: ", submissionId, "\n"
        )
        return(invisible(FALSE))
    }

    abort_workflow <- terra$abortSubmission(
        workspaceNamespace = namespace,
        workspaceName = URLencode(name),
        submissionId = submissionId)
    .avstop_for_status(abort_workflow, "avworkflow_stop (abort workflow)")

    invisible(TRUE)
}
