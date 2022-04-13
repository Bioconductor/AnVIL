.DRS_MARTHA <- "https://us-central1-broad-dsde-prod.cloudfunctions.net/martha_v3"

.DRS_RAWLS <- paste0(
    "https://rawls.dsde-prod.broadinstitute.org/api/workspaces/",
    "%s/%s/enableRequesterPaysForLinkedServiceAccounts"
)

.DRS_STAT_TEMPLATE <- list(
    drs = character(),
    fileName = character(),
    size = integer(),
    gsUri = character(),
    accessUrl = character(),
    timeUpdated = character(),
    hashes = list(),
    bucket = character(),
    name = character(),
    googleServiceAccount = list()
)

.drs_is_uri <-
    function(source)
{
    .is_character(source) & grepl("drs://[^/]+", source)
}

#' @importFrom httr POST add_headers
.martha_v3 <-
    function(drs, template, access_token)
{
    headers <- add_headers(
        Authorization = paste("Bearer", access_token),
        "content-type" = "application/json"
    )

    body <- list(
        fields = setdiff(names(template), "drs"),
        url = jsonlite::unbox(drs)
    )
    body_json <- jsonlite::toJSON(body)
    response <- POST(.DRS_MARTHA, headers, body = body_json, encode="raw")
    .avstop_for_status(response, "DRS resolution")

    ## add drs field to response
    lst <- c(as.list(response), list(drs = drs))

    ## nest list elements so length == 1L
    is_list <-
        vapply(lst, is.list, logical(1))
    lst[is_list] <- lapply(lst[is_list], list)

    ## return as tibble
    tbl <- as_tibble(lst[lengths(lst) == 1L])
    .tbl_with_template(tbl, template)
}

.drs_stat_impl <-
    function(source, template)
{
    access_token <- .gcloud_access_token("drs")

    if (identical(.Platform$OS.type, "windows")) {
        mc.cores <- 1L
    } else {
        mc.cores <- getOption("mc.cores", max(min(length(source), 8L), 1L))
    }
    response <- withCallingHandlers({
        mclapply(
            source, .martha_v3, template, access_token,
            mc.cores = mc.cores
        )
    }, warning = function(condition) {
        ## handled below
        invokeRestart("muffleWarning")
    })
    if (!length(response))
        ## length(source) == 0L
        response <- list(
            .tbl_with_template(as_tibble(list()), template)
        )

    ## validate response
    ok <- vapply(response, inherits, logical(1), "tbl_df")
    if (!all(ok)) {
        first_error_idx <- head(which(!ok), 1L)
        first_error <-
            conditionMessage(attr(response[[first_error_idx]], "condition"))
        warning(
            "failed to resolve ", sum(!ok), " DRS url(s):\n",
            "url(s):\n",
            "  ", paste(source[!ok], collapse = "\n"), "\n",
            "first error:\n",
            first_error
        )
        ## remember ok and failed drs rows
        failed_drs <- .tbl_with_template(tibble(drs = source[!ok]), template)
        response <- c(response[ok], list(failed_drs))
    }

    ## ensure tbl order matches input order
    tbl <- bind_rows(response)
    order_idx <- match(source, tbl$drs)
    tbl <- tbl[order_idx, ]
    class(tbl) <- c("drs_stat_tbl", class(tbl))
    tbl
}

#' @rdname drs
#' @md
#'
#' @title DRS (Data Repository Service) URL management
#'
#' @description `drs_stat()` resolves zero or more DRS URLs to their
#'     google bucket location.
#'
#' @details `drs_stat()` sends requests in parallel to the DRS server,
#'     using 8 forked processes (by default) to speed up queries. Use
#'     `options(mc.cores = 16L)`, for instance, to set the number of
#'     processes to use.
#'
#'     `drs_stat()` uses the AnVIL 'pet' account associated with a
#'     runtime. The pet account is discovered by default when
#'     evaluated on an AnVIL runtime (e.g., in RStudio or a Jupyter
#'     notebook in the AnVIL), or can be found in the return value of
#'     `avruntimes()`.
#'
#' @param source character() DRS URLs (beginning with 'drs://') to
#'     resources managed by the 'martha' DRS resolution server.
#'
#' @return `drs_stat()` returns a tbl with the following columns:
#'
#' - fileName: character() (resolver sometimes returns null).
#' - size: integer() (resolver sometimes returns null).
#' - contentType: character() (resolver sometimes returns null).
#' - gsUri: character() (resolver sometimes returns null).
#' - timeCreated: character() (the time created formatted using ISO
#'   8601; resolver sometimes returns null).
#' - timeUpdated: character() (the time updated formatted using ISO
#'   8601; resolver sometimes returns null).
#' - bucket: character() (resolver sometimes returns null).
#' - name: character() (resolver sometimes returns null).
#' - googleServiceAccount: list() (null unless the DOS url belongs to
#'   a Bond supported host).
#' - hashes: list() (contains the hashes type and their checksum
#'   value; if unknown. it returns null)
#'
#' @examples
#' drs_eg_anvil <- c(
#'     "drs://dg.ANV0/975bd45f-f022-4fad-b9a2-3a00c3b8792c",
#'     "drs://dg.ANV0/00008531-03d7-418c-b3d3-b7b22b5381a0"
#' )
#'
#' if (gcloud_exists() && startsWith(gcloud_account(), "pet-")) {
#'     ## from within AnVIL
#'     drs_stat(drs_eg_anvil)
#' }
#'
#' @importFrom rlang .data
#'
#' @importFrom parallel mclapply
#'
#' @export
drs_stat <-
    function(source = character())
{
    stopifnot(
        `'source' must be DRS URIs, i.e., starting with "drs://"` =
            all(.drs_is_uri(source))
    )

    tbl <- .drs_stat_impl(source, .DRS_STAT_TEMPLATE)

    ## accessUrl
    needAccess <- tbl |> filter(is.na(.data$accessUrl))
    access_url <- .drs_access_url(needAccess)
    tbl |>
        mutate(
            accessUrl = as.character(ifelse(
                is.na(.data$accessUrl), access_url, .data$accessUrl
            ))
        ) |>
        select(-c("googleServiceAccount"))
}

.drs_access_url_1 <-
    function(gsUri, data, project)
{
    ## write the service account credentials to a temporary file
    key_file <- tempfile()
    on.exit(unlink(key_file))
    writeLines(jsonlite::toJSON(data[[1]], auto_unbox = TRUE), key_file)

    ## invoke gsutil signurl
    response <- .gsutil_do(c(
        "signurl",
        "-r", "US", # multi-region US location; probably not correct
        "-b", project, # project to be billed
        key_file, gsUri
    ))

    ## signed url in last line, fourth tab-delimited field
    strsplit(tail(response, 1L), "\t")[[1]][[4]]
}

.drs_access_url <-
    function(tbl)
{
    as.character(unlist(
        Map(
            .drs_access_url_1, tbl$gsUri, tbl$googleServiceAccount,
            MoreArgs = list(project = gcloud_project())
        ),
        use.names = FALSE
    ))
}

#' @rdname drs
#'
#' @description `drs_access_url()` returns a vector of 'signed' URLs
#'     that allow access to restricted resources via standard https
#'     protocols.
#'
#' @return `drs_access_url()` returns a vector of https URLs
#'     corresponding to the vector of DRS URIs provided as inputs to
#'     the function.
#'
#' @export
drs_access_url <-
    function(source = character())
{
    stopifnot(
        `'source' must be DRS URIs, i.e., starting with "drs://"` =
            all(.drs_is_uri(source))
    )

    template_idx <- c("drs", "gsUri", "googleServiceAccount")
    template <- .DRS_STAT_TEMPLATE[template_idx]
    tbl <- .drs_stat_impl(source, template)
    .drs_access_url(tbl)
}

.drs_check_local_path_exists <-
    function(path)
{
    file_exists <- file.exists(path)
    if (any(file_exists)) {
        stop(
            "'destination' file paths already exist:",
            "\n  ", paste0(path[file_exists], collapse = "\n  ")
        )
    }
}

.drs_cp <-
    function(source, destination, ..., FUN)
{
    Map(function(source, destination, ...) {
        status <- "OK"
        tryCatch({
            message(basename(destination), " ", appendLF = FALSE)
            if (.is_local_directory(dirname(destination)))
                .drs_check_local_path_exists(destination)
            FUN(source, destination, ...)
        }, error = function(err) {
            msg <- paste0(
                "failed to download drs resource:",
                "\n  source: ", source,
                "\n  reason: ", conditionMessage(err)
            )
            warning(msg, call. = FALSE)
            status <<- "FAIL"
            NULL
        })
        message("[", status, "]")
    }, source, destination, ...)
}

.drs_download <-
    function(tbl, destination)
{
    tbl <-
        tbl |>
        filter( .is_https(tbl$url) & .is_local_directory(destination) )
    if (!nrow(tbl))
        return(tbl)

    path <- file.path(destination, tbl$fileName)
    .drs_cp(tbl$url, path, MoreArgs = list(quiet = TRUE), FUN = download.file)

    mutate(tbl, destination = path)
}

.drs_gsutil_cp <-
    function(tbl, destination)
{
    tbl <-
        tbl |>
        filter( !(.is_https(tbl$url) & .is_local_directory(destination)) )

    if (!nrow(tbl))
        return(tbl)

    path <- paste(destination, tbl$fileName, sep = "/")
    .drs_cp(tbl$url, path, FUN = gsutil_cp)
    mutate(tbl, destination = path)
}

#' @rdname drs
#' @md
#'
#' @description `drs_cp()` copies 0 or more DRS URIs to a google
#'     bucket or local folder
#'
#' @param destination character(1) directory path in which to retrieve
#'     files.
#'
#' @param ... additional arguments, passed to `gsutil_cp()` for file
#'     copying.
#'
#' @return `drs_cp()` returns a tibble like `drs_stat()`, but with
#'     additional columns
#'
#' - simple: logical() value indicating whether resolution used a
#'   simple signed URL (`TRUE`) or auxilliary service account.
#' - destination: character() full path to retrieved object(s)
#'
#' @examples
#'
#' @export
drs_cp <- function(source, destination, ...)
    UseMethod("drs_cp")

#' @export
drs_cp.drs_stat_tbl <-
    function(source, destination, ...)
{
    stopifnot(
        `'destination' must be a google bucket or existing local directory` =
            .gsutil_is_uri(destination) || .is_local_directory(destination)
    )
    destination <- sub("/$", "", destination)
    tbl <- source

    ## download (https to local file system) or gsutil_cp each resource
    tbl <- bind_rows(
        .drs_download(tbl, destination),
        .drs_gsutil_cp(tbl, destination)
    )
    template <- c(.DRS_STAT_TEMPLATE, list(destination = character()))
    tbl <- .tbl_with_template(tbl, template)
    order_idx <- match(source$drs, tbl$drs)
    tbl[order_idx, ]
}

#' @export
drs_cp.character <-
    function(source, destination, ...)
{
    stopifnot(
        `'source' must be DRS URIs, e.g., starting with "drs://"` =
            all(.drs_is_uri(source)),
        `'destination' must be a google bucket or existing local directory` =
            .gsutil_is_uri(destination) || .is_local_directory(destination)
    )

    tbl <- drs_stat(source)
    drs_cp(tbl, destination, ...)
}
