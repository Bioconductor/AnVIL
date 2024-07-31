#' @name utilities
#'
#' @title Utilities for managing library paths
#'
#' @description `add_libpaths()`: Add local library paths to
#'     `.libPaths()`.
#'
#' @param paths `character()`: vector of directories to add to
#'     `.libPaths()`. Paths that do not exist will be created.
#'
#' @return `add_libpaths()`: updated .libPaths(), invisibly.
#'
#' @examples
#' \dontrun{add_libpaths("/tmp/host-site-library")}
#'
#' @export
add_libpaths <-
    function(paths)
{
    stopifnot(is.character(paths))

    ## make sure all paths exist
    exist <- vapply(paths, dir.exists, logical(1))
    ok <- vapply(paths[!exist], dir.create, logical(1))
    if (!all(ok))
        stop(
            "'add_libpaths()' failed to create directories:\n",
            "  '", paste(paths[!exist][!ok], collapse="'\n  '"), "'"
        )

    .libPaths(c(paths, .libPaths()))
}

#' @name life_cycle
#'
#' @title Internal helpers
#'
#' @description These functions are not intended to be called directly by users.
#'   They are used internally by the package.
#'
#' @param newfun The name of the function to use instead. It can be a specific
#'   function within another package (e.g., `AnVIL::avstorage`) or a function in
#'   the current package (e.g., `avstorage`).
#'
#' @param newpackage If a function is moved to a new package, the name of the
#'   new package can be specified here. This is equivalent to specifying
#'   `newfun = paste0(newpackage, "::", newfun)`.
#'
#' @param package The name of the package where the deprecated function resides.
#'
#' @param cycle The life cycle stage of the function. This can be either
#'   `"deprecated"` or `"defunct"`.
#'
#' @param title The Rd name prefix of the documentation page where the
#'   deprecation is documented (e.g., "av" for "av-deprecated").
#'
#' @keywords internal
.life_cycle <- function(
    newfun = oldfun, newpackage, package = "AnVIL",
    cycle = c("deprecated", "defunct"), title = package
) {
    removal <- missing(newfun) && missing(newpackage)
    oldfun <- as.character(sys.call(sys.parent())[1L])
    cycle <- match.arg(cycle)

    if (!missing(newpackage))
        newfun <- paste0(newpackage, "::", gsub("^.*::", "", newfun))

    msg <- c(
        gettextf("'%s' is %s.\n", oldfun, cycle),
        if (!removal) gettextf("Use '%s' instead.\n", newfun),
        gettextf("See help('%s').",  paste0(title, "-", cycle))
    )
    cycle_fun <- switch(cycle, deprecated = .Deprecated, defunct = .Defunct)
    arglist <- list(new = newfun, msg = msg, package = package)
    do.call(cycle_fun, arglist)
}

isScalarCharacter_or_NULL <- function(x, na.ok = FALSE, zchar = FALSE)
    isScalarCharacter(x, na.ok, zchar) || is.null(x)

.is_local_directory <- function(x)
    isScalarCharacter(x) && dir.exists(x)

.is_https <- function(x)
    isCharacter(x) & startsWith(x, "https://")

.is_workspace <-
    function(x)
{
    isScalarCharacter(x) &&
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
            full_join(tbl, result, by = have) |>
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
