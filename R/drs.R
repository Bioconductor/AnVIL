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
    isCharacter(source) & grepl("drs://[^/]+", source)
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
    avstop_for_status(response, "DRS resolution")

    ## add drs field to response
    lst <- c(as.list(response), list(drs = drs))

    ## unbox accessUrl; if accessUrl == NULL, then this is a no-op
    lst$accessUrl <- unlist(lst$accessUrl, use.names = FALSE)

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
            "  url(s):\n",
            "    ", paste(source[!ok], collapse = "\n    "), "\n",
            "first error:\n  ",
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

#' @name drs-deprecated
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
#' `drs_stat()` uses the AnVIL 'pet' account associated with a
#' runtime. The pet account is discovered by default when evaluated on
#' an AnVIL runtime (e.g., in RStudio or a Jupyter notebook in the
#' AnVIL), or can be found in the return value of `avruntimes()`.
#'
#' Errors reported by the DRS service are communicated to the user,
#' but can be cryptic. The DRS service itself is called
#' 'martha'. Errors mentioning martha might commonly involve a
#' mal-formed DRS uri. Martha uses a service called 'bond' to
#' establish credentials with registered third party entities such as
#' Kids First. Errors mentioning bond might involve absence of
#' credentials, within Terra, to access the resource; check that, in
#' the Terra / AnVIL graphical user interface, the user profiles
#' 'External Entities' includes the organization to which the DRS uri
#' is being resolved.
#'
#' @param source character() DRS URLs (beginning with 'drs://') to
#'     resources managed by the 'martha' DRS resolution server.
#'
#' @param region character(1) Google cloud 'region' in which the DRS
#'     resource is located. Most data is located in \code{"US"} (the
#'     default); in principle \code{"auto"} allows for discovery of
#'     the region, but sometimes fails. Regions are enumerated at
#'     \url{https://cloud.google.com/storage/docs/locations#available-locations}.
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
#' drs <- c(
#'     vcf = "drs://dg.ANV0/6f633518-f2de-4460-aaa4-a27ee6138ab5",
#'     tbi = "drs://dg.ANV0/4fb9e77f-c92a-4deb-ac90-db007dc633aa"
#' )
#'
#' if (gcloud_exists() && startsWith(gcloud_account(), "pet-")) {
#'     ## from within AnVIL
#'     tbl <- drs_stat(uri)
#'     urls <- drs_access_url(uri)
#'     ## library(VariantAnnotation)
#'     ## vcffile <- VcfFile(urls[["vcf"]], urls[["tbi"]])
#'     ##
#'     ## header <- scanVcfHeader(vcffile)
#'     ## meta(header)[["contig"]]
#' }
#'
#' @importFrom rlang .data
#'
#' @importFrom parallel mclapply
#'
#' @export
drs_stat <-
    function(source = character(), region = "US")
{
    stopifnot(
        `'source' must be DRS URIs, i.e., starting with "drs://"` =
            all(.drs_is_uri(source))
    )
    .life_cycle(
        newpackage = "AnVILGCP",
        title = "drs"
    )
    tbl <- .drs_stat_impl(source, .DRS_STAT_TEMPLATE)

    select(tbl, -c("googleServiceAccount"))
}

.drs_access_url_1 <-
    function(gsUri, data, project, region)
{
    ## write the service account credentials to a temporary file
    key_file <- tempfile()
    on.exit(unlink(key_file))
    writeLines(jsonlite::toJSON(data[[1]], auto_unbox = TRUE), key_file)

    ## invoke gsutil signurl
    response <- .gsutil_do(c(
        "signurl",
        "-r", region,  # 'auto' fails when not using a service account
        "-b", project, # project to be billed
        key_file, gsUri
    ))

    ## signed url in last line, fourth tab-delimited field
    strsplit(tail(response, 1L), "\t")[[1]][[4]]
}

.drs_access_url <-
    function(tbl, region)
{
    as.character(unlist(
        Map(
            .drs_access_url_1, tbl$gsUri, tbl$googleServiceAccount,
            MoreArgs = list(project = gcloud_project(), region = region)
        ),
        use.names = FALSE
    ))
}

#' @rdname drs-deprecated
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
    function(source = character(), region = "US")
{
    stopifnot(
        `'source' must be DRS URIs, i.e., starting with "drs://"` =
            all(.drs_is_uri(source))
    )
    .life_cycle(
        newpackage = "AnVILGCP",
        title = "drs"
    )
    template_idx <- c("drs", "gsUri", "googleServiceAccount")
    template <- .DRS_STAT_TEMPLATE[template_idx]
    tbl <- .drs_stat_impl(source, template)
    result <- rep(NA_character_, NROW(tbl))

    available <- lengths(tbl$googleServiceAccount) != 0L
    if (!all(available))
        warning(
            "'source' not available as signed URLs:\n",
            paste0("  '", source[!available], "'\n"),
            call. = FALSE
        )
    result[available] <- .drs_access_url(filter(tbl, available), region)
    if (!is.null(names(source)))
        names(result) <- names(source)
    result
}

#' @rdname drs-deprecated
#'
#' @description `drs_cp()` copies 0 or more DRS URIs to a google
#'     bucket or local folder
#'
#' @param destination `character(1)`, google cloud bucket or local
#'     file system destination path.
#'
#' @param ... additional arguments, passed to `gsutil_cp()` for file
#'     copying.
#'
#' @param overwrite logical(1) indicating that source `fileName`s
#'     present in `destination` should downloaded again.
#'
#' @return `drs_cp()` returns a tibble like `drs_stat()`, but with
#'     additional columns
#'
#' - simple: logical() value indicating whether resolution used a
#'   simple signed URL (`TRUE`) or auxilliary service account.
#' - destination: character() full path to retrieved object(s)
#'
#' @export
drs_cp <- function(source, destination, ..., overwrite = FALSE) {
    .life_cycle(
        newpackage = "AnVILGCP",
        title = "drs"
    )
    UseMethod("drs_cp")
}

#' @export
drs_cp.drs_stat_tbl <-
    function(source, destination, ..., overwrite = FALSE)
{
    stopifnot(
        `'source' contains duplicate 'fileName's` =
            anyDuplicated(source$fileName) == 0L,
        `'destination' must be a google bucket or existing local directory` =
            .gsutil_is_uri(destination) || .is_local_directory(destination),
        isScalarLogical(overwrite)
    )
    destination <- sub("/$", "", destination)
    tbl <- source

    ## use gsutil_cp to copy each resource
    result <- gsutil_cp(
        tbl$gsUri,
        destination,
        if (!overwrite) "-n",
        ...
    )

    ## add to drs_stat_tbl
    tbl <-
        tbl |>
        mutate(destination = file.path(destination, basename(.data$gsUri)))

    tbl
}

#' @export
drs_cp.character <-
    function(source, destination, ..., region = "US", overwrite = FALSE)
{
    stopifnot(
        `'source' must be DRS URIs, e.g., starting with "drs://"` =
            all(.drs_is_uri(source)),
        `'destination' must be a google bucket or existing local directory` =
            .gsutil_is_uri(destination) || .is_local_directory(destination)
    )

    tbl <- drs_stat(source, region)
    drs_cp(tbl, destination, ..., overwrite = overwrite)
}
