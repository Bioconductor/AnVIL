.is_character <-
    function(x, na.ok = FALSE, zchar = FALSE)
{
    is.character(x) &&
        (na.ok || all(!is.na(x))) &&
        (zchar || all(nzchar(x)))
}

.is_scalar_character <- function(x, na.ok = FALSE, zchar = FALSE)
    length(x) == 1L && .is_character(x, na.ok, zchar)

.is_scalar_character_or_NULL <- function(x, na.ok = FALSE, zchar = FALSE)
    .is_scalar_character(x, na.ok, zchar) || is.null(x)

.is_character_0_or_1 <-
    function(x, na.ok = FALSE, zchar = FALSE)
{
    (length(x) == 0L || length(x) == 1L) &&
        .is_character(x, na.ok, zchar)
}

.is_scalar_logical <- function(x, na.ok = FALSE)
    is.logical(x) && length(x) == 1L && (na.ok || !is.na(x))

.is_scalar_integer <- function(x, na.ok = FALSE)
    is.integer(x) && length(x) == 1L && (na.ok || !is.na(x))

.is_scalar_numeric <- function(x, na.ok = FALSE, infinite.ok = FALSE)
    is.numeric(x) && length(x) == 1L &&
        (na.ok || !is.na(x)) &&
        (infinite.ok || is.finite(x))

.is_local_directory <- function(x)
    .is_scalar_character(x) && dir.exists(x)

.is_https <- function(x)
    .is_character(x) & startsWith(x, "https://")

.is_workspace <-
    function(x)
{
    .is_scalar_character(x) &&
        ## exactly 1 `/`
        identical(lengths(regmatches(x, gregexpr("/", x, fixed = TRUE))), 1L)
}

#' @importFrom dplyr full_join
.tbl_with_template <-
    function(tbl, tmpl)
{
    result <- as_tibble(tmpl)
    if (nrow(tbl)) {
        have <- intersect(names(tbl), names(tmpl))
        tbl <- select(tbl, have)
        result <-
            full_join(tbl, result, by = have) %>%
            select(names(tmpl))
    }
    result
}

#' @importFrom utils head
.pretty <- function(x, indent = 2, exdent = 0, some=FALSE) {
    len <- length(x)
    if (some && len > 6)
        x <- head(x, 5)
    pad <- paste0(rep(" ", indent), collapse="")
    paste(c(
        strwrap(paste(x, collapse=", "), indent = indent, exdent = exdent),
        if (some && len > 6)
            paste0(pad, "# ... with ", len, " more elements")
    ), collapse = "\n")
}

.pretty_text <- function(..., indent = 0L, exdent = 0L) {
    text <- paste(..., collapse = " ")
    paste(strwrap(text, indent = indent, exdent = exdent), collapse = "\n")
}

##
## internal
##

#' @importFrom httr status_code http_condition headers
.avstop_for_status <-
    function(response, op)
{
    status <- status_code(response)
    if (status < 400L)
        return(invisible(response))

    cond <- http_condition(status, "error")
    type <- headers(response)[["content-type"]]
    msg <- NULL
    if (nzchar(type) && grepl("application/json", type)) {
        content <- as.list(response)
        msg <- content[["message"]]
        if (is.null(msg))
            ## e.g., from bond DRS server
            msg <- content$response$text
    } else if (nzchar(type) && grepl("text/html", type)) {
        ## these pages can be too long for a standard 'stop()' message
        cat(as.character(response), file = stderr())
    }

    message <- paste0(
        "'", op, "' failed:\n  ",
        conditionMessage(cond),
        if (!is.null(msg)) "\n  ", msg
    )
    stop(message, call.=FALSE)
}
