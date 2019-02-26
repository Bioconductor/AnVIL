.is_scalar_character <- function(x, na.ok = FALSE)
    is.character(x) && length(x) == 1L && (na.ok || !is.na(x)) && nzchar(x)

.is_scalar_logical <- function(x, na.ok = FALSE)
    is.logical(x) && length(x) == 1L && (na.ok || !is.na(x))

.pretty <- function(x, indent = 2, exdent = 0)
    paste(
        strwrap(paste(x, collapse=", "), indent = indent, exdent = exdent),
        collapse = "\n"
    )
