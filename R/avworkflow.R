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
      failed = failed,
      submissionRoot = x[["submissionRoot"]]
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
            failed = integer(),
            submissionRoot = character()
        )
    }

    bind_rows(submissions) |>
        mutate(
            submissionDate = .POSIXct(as.numeric(
                as.POSIXct(.data$submissionDate, "%FT%T", tz="UTC")
            )),
            namespace = namespace,
            name = name
        ) |>
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

.avworkflow_files_from_api <-
    function(response, submissionId, namespace, name)
{
    ## find id's of each workflow
    workflowIds <- unlist(lapply(response$workflows, function(workflow) {
        ## may be 'NULL' if submission aborted before workflow starts
        workflow$workflowId
    }))

    if (is.null(workflowIds)) {
        stop(
            "no workflows available\n",
            "  submissionId: '", submissionId, "'"
        )
    }

    tbl <- tibble(
        file = character(), workflow = character(), task = character(),
        type = character(), path = character(),
        submissionId = character(), workflowId = character()
    )
    for (workflowId in workflowIds) {
        ## query for outputs
        outputs <- Terra()$workflowOutputsInSubmission(
            namespace, URLencode(name), submissionId, workflowId
        )
        if (identical(status_code(outputs), 404L)) {
            ## no submissionId / workflowId association
            return(NULL)
        }
        .avstop_for_status(
            outputs, "avworkflow_files() 'workflowOutputsInSubmission'"
        )
        response <- content(outputs)

        tasks <- names(response$tasks)
        ## output files
        outputs <- lapply(response$tasks, `[[`, "outputs")
        outputs_per_task <- unlist(lapply(outputs, function(output) {
            length(unlist(output, use.names = FALSE))
        }), use.names = FALSE)
        outputs <- unlist(outputs, use.names = FALSE)
        ## log files
        logs <- lapply(response$tasks, `[[`, "logs")
        logs_per_task <- unlist(lapply(logs, function(log) {
            length(unlist(log, use.names = FALSE))
        }), use.names = FALSE)
        logs <- unlist(logs, use.names = FALSE)

        ## prepare return tibble
        path <- c(outputs, logs)
        workflow <- unique(sub("\\..*", "", tasks))
        tasks <- sub("[^\\.]*\\.?(.*)", "\\1", tasks)
        tasks[!nzchar(tasks)] <- NA_character_
        task <- c(
            rep(tasks, outputs_per_task),
            rep(tasks, logs_per_task)
        )
        if (length(workflow) != 1L) {
            workflow <- workflow[[1L]]
            warning(
                "cannot guess workflow name; using '", workflow, "'\n",
                "  submissionId: ", submissionId
            )
        }
        type <- rep(c("output", "control"), c(length(outputs), length(logs)))
        tbl0 <- tibble(
            file = basename(path),
            workflow = workflow,
            task = task,
            type = type,
            path = path,
            submissionId = submissionId,
            workflowId = workflowId
        )
        tbl <- bind_rows(tbl, tbl0)
    }
    tbl
}

.avworkflow_files_from_submissionRoot <-
    function(submissionId, submissionRoot)
{
    ## scrape bucket submissionRoot objects for output files
    if (!gsutil_exists(submissionRoot)) {
        stop(
            "'avworkflow_files()' submissionId produced no objects\n",
            "    submissionId: '", submissionId, "'"
        )
    }
    path <- gsutil_ls(submissionRoot, recursive = TRUE)

    ## submissionRoot/submissionId/workflow/workflowId/task/...
    objects <- substr(path, nchar(submissionRoot) + 2L, nchar(path))
    parts <- strsplit(objects, "/")
    workflow <- vapply(parts, `[[`, character(1), 1L)
    workflow_id <- task <- rep(NA_character_, length(workflow))
    idx <- workflow != .WORKFLOW_LOGS
    workflow_id <- vapply(parts, `[[`, character(1), 2L)
    workflow_id[!idx] <- sub(".*\\.(.+)\\..*", "\\1", workflow_id[!idx])
    task[idx] <- vapply(parts[idx], `[[`, character(1), 3L)

    ## tbl of available files
    tibble(
        file = basename(path),
        workflow = workflow,
        task = task,
        type = .avworkflow_file_type(file, workflow),
        path = path,
        submissionId = submissionId,
        workflowId = workflow_id
    )
}

.avworkflow_files_from_submissionId <-
    function(submissionId, namespace, name)
{
    stopifnot(
        .is_scalar_character(submissionId),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    ## find the submission
    monitor <- Terra()$monitorSubmission(
        namespace, URLencode(name), submissionId
    )
    .avstop_for_status(monitor, "avworkflow_files() 'monitorSubmission'")
    response <- content(monitor)

    ## FIXME -- how to know if information on outputs from workflow?
    ## 'submission' in submissionRoot?
    submissionRoot <- response$submissionRoot
    has_workflow_outputs <- grepl("/submissions/", submissionRoot)

    if (has_workflow_outputs) {
        ## query API for output file information
        tbl <- .avworkflow_files_from_api(
            response, submissionId, namespace, name
        )
    } else {
        ## scrape submissionRoot objects for output file information
        tbl <- .avworkflow_files_from_submissionRoot(
            submissionId, submissionRoot
        )
    }

    bind_cols(
        tbl,
        submissionRoot = submissionRoot,
        namespace = namespace,
        name = name
    )
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
#' @param bucket character(1) DEPRECATED (ignored in the current
#'     release) name of the google bucket in which the workflow
#'     products are available, as `gs://...`. Usually the bucket of
#'     the active workspace, returned by `avbucket()`.
#'
#' @details For `avworkflow_files()`, the `submissionId` is the
#'     identifier associated with the submission of one (or more)
#'     workflows, and is present in the return value of
#'     `avworkflow_jobs()`; the example illustrates how the first row
#'     of `avworkflow_jobs()` (i.e., the most recently completed
#'     workflow) can be used as input to `avworkflow_files()`. When
#'     `submissionId` is not provided, the return value is for the
#'     most recently submitted workflow of the namespace and name of
#'     `avworkspace()`.
#'
#' @return `avworkflow_files()` returns a tibble with columns
#'
#' - file: character() 'base name' of the file in the bucket.
#' - workflow: character() name of the workflow the file is associated
#'   with.
#' - task: character() name of the task in the workflow that generated
#'   the file.
#' - path: charcter() full path to the file in the google bucket.
#' - submissionId: character() internal identifier associated with the
#'   submission the files belong to.
#' - workflowId: character() internal identifer associated with each
#'   workflow (e.g., row of an avtable() used as input) in the
#'   submission.
#' - submissionRoot: character() path in the workspace bucket to the root
#'   of files created by this submission.
#' - namespace: character() AnVIL workspace namespace (billing account)
#'   associated with the submissionId.
#' - name: character(1) AnVIL workspace name associated with the
#'   submissionId.
#'
#' @importFrom tibble is_tibble
#'
#' @examples
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#'     ## e.g., from within AnVIL
#'     avworkflow_jobs() |>
#'     ## select most recent workflow
#'     head(1) |>
#'     ## find paths to output and log files on the bucket
#'     avworkflow_files()
#' }
#'
#' @export
avworkflow_files <-
    function(submissionId = NULL, bucket = avbucket(),
             namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    if (!missing(bucket)) {
        warning(.pretty_text(
            "'bucket=' is deprecated; it is ignored in the current ",
            "implementation and will be removed in a subsequent release; ",
            "provide workspace 'namespace=' and 'name=' arguments to ",
            "'avworkflow_files()' directly"
        ), call. = FALSE)
    }
    stopifnot(
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    if (is_tibble(submissionId)) {
        stopifnot()
    } else if (.is_character(submissionId)) {
        submissionId <- tibble(
            submissionId = submissionId,
            namespace = namespace,
            name = name
        )
    } else if (is.null(submissionId)) {
        submissionId <-
            avworkflow_jobs(namespace = namespace, name = name) |>
            ## default: most recent workflow job
            head(1)
    } else {
        stop(.pretty_text(
            "'submissionId' must be NULL, character(1), or a tibble ",
            " with a single row and columns 'submissionId', 'namespace', ",
            "and 'name'"
        ))
    }

    tbl <- .avworkflow_files_from_submissionId(
        submissionId$submissionId,
        submissionId$namespace,
        submissionId$name
    )

    tbl |>
        arrange(
            match(.data$type, c("output", "control")), # output first
            .data$workflowId, .data$task, .data$file, .data$path
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

    fls <- avworkflow_files(submissionId)

    source <- pull(fls, "submissionRoot") |> unique()
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
            "use 'dry = FALSE' to localize ", n_files, " workflow files"
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
#' @param config a `avworkflow_configuration` object of the workflow
#'     that will be run. Only `entityType` and method configuration
#'     name and namespace are used from `config`; other configuration
#'     values must be communicated to AnVIL using
#'     `avworkflow_configuration_set()`.
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
