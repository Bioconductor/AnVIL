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
      succeeded = succeeded,
      failed = failed
    )
}

#' @rdname avworkflow
#'
#' @title Working with AnVIL Workflow submissions and file outputs
#' @md
#'
#' @description `avworkflow_jobs()` returns a tibble summarizing
#'     submitted workflow jobs for a namespace and name.
#'
#' @inheritParams avworkspace
#'
#' @return `avworkflow_jobs()` returns a tibble, sorted by
#'     `submissionDate`, with columns
#'
#' - submissionId character() job identifier from the workflow runner.
#' - submitter character() AnVIL user id of individual submitting the job.
#' - submissionDate POSIXct() date (in local time zone) of job submission.
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

#' @rdname avworkflow
#' @md
#'
#' @description `avworkflow_files()` returns a tibble containing
#'     information and file paths to workflow outputs.
#'
#' @param submissionId a character() of workflow submission ids or a
#'     tibble with column `submissionId`. See 'Details'.
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
#'     `avworkflow_files()`.
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
avworkflow_files <-
    function(submissionId, bucket = avbucket())
{
    WORKFLOW_LOGS <- "workflow.logs"

    stopifnot(
        .is_scalar_character(bucket),
        .is_character(submissionId) ||
        (is_tibble(submissionId) && "submissionId" %in% names(submissionId))
    )
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
    idx <- workflow != WORKFLOW_LOGS
    task[idx] <- vapply(part[idx], `[[`, character(1), 7)
    tbl <-  tibble(
        file = basename(path),
        workflow = workflow,
        task = task,
        path = path
    )
    tbl %>%
        arrange(
            ## workflows.log last
            workflow == WORKFLOW_LOGS, task, path, file
        )
}
