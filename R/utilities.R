.is_scalar_character <- function(x, na.ok = FALSE, zchar = FALSE)
    is.character(x) && length(x) == 1L &&
        (na.ok || !is.na(x)) &&
        (zchar || nzchar(x))

.is_character_0_or_1 <-
    function(x, na.ok = FALSE, zchar = FALSE)
{
    (length(x) == 0L && is.character(x)) ||
        .is_scalar_character(x, na.ok, zchar)
}

.is_scalar_logical <- function(x, na.ok = FALSE)
    is.logical(x) && length(x) == 1L && (na.ok || !is.na(x))

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
