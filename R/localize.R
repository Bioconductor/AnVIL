#' @rdname localize
#'
#' @title Copy packages, folders, or files to or from google buckets.
#'
#' @description `localize()`: recursively synchronizes files from a
#'     Google storage bucket (`source`) to the local file system
#'     (`destination`). This command acts recursively on the `source`
#'     directory, and does not delete files in `destination` that are
#'     not in `source.
#'
#' @param source `character(1)`, a google storage bucket or local file
#'     system directory location.
#'
#' @param destination `character(1)`, a google storage bucket or local
#'     file system directory location.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `localize()`: exit status of function `gsutil_rsync()`.
#'
#' @export
localize <-
    function(source, destination, dry = TRUE)
{
    stopifnot(
        .gsutil_is_uri(source),
        .is_scalar_character(destination), dir.exists(destination),
        .is_scalar_logical(dry)
    )

    ## FIXME: return destination paths of copied files
    gsutil_rsync(
        source, destination, delete = FALSE, recursive = TRUE, dry = dry
    )
}

#' @rdname localize
#'
#' @description `delocalize()`: synchronize files from a local file
#'     system (`source`) to a Google storage bucket
#'     (`destination`). This command acts recursively on the `source`
#'     directory, and does not delete files in `destination` that are
#'     not in `source`.
#'
#' @param unlink `logical(1)` remove (unlink) the file or directory
#'     in `source`. Default: `FALSE`.
#'
#' @return `delocalize()`: exit status of function `gsutil_rsync()`
#'
#' @export
delocalize <-
    function(source, destination, unlink = FALSE, dry = TRUE)
{
    stopifnot(
        .is_scalar_character(source), file.exists(source),
        .gsutil_is_uri(destination),
        .is_scalar_logical(unlink),
        .is_scalar_logical(dry)
    )
    ## sync and optionally remove source
    result <- gsutil_rsync(
        source, destination, delete = FALSE, recursive = TRUE, dry = dry
    )
    if (!dry && unlink)
        unlink(source, recursive=TRUE)
    result
}

#' @rdname localize
#'
#' @description `install()`: install R / Bioconductor packages, using
#'     fast pre-built 'binary' libraries if available.
#'
#' @param pkgs `character()` packages to install from binary repository.
#'
#' @param lib `character(1)` library path (directory) in which to
#'     install `pkgs`; defaults to `.libPaths()[1]`.
#'
#' @param ... additional arguments, passed to `install.packages()`.
#'
#' @param version `character(1)` or `package_version` Bioconductor
#'     version, e.g., "3.12".
#'
#' @param binary_base_url `character(1)` host and base path for binary
#'     package 'CRAN-style' repository; not usually required by the
#'     end-user.
#'
#' @param verbose `logical(1)` report on package installation
#'     progress?
#'
#' @return `install()`: return value of `install.packages()`.
#'
#' @examples
#' \dontrun{install(c('BiocParallel', 'BiocGenerics'))}
#'
#' @importFrom utils contrib.url install.packages
#'
#' @export
install <-
    function(pkgs, lib = .libPaths()[1], ...,
        version = BiocManager::version(),
        binary_base_url = "https://storage.googleapis.com",
        verbose = getOption("verbose"))
{
    stopifnot(
        .is_character(pkgs),
        .is_scalar_character(lib), dir.exists(lib),
        .is_scalar_character(version) || is.package_version(version),
        .is_scalar_character(binary_base_url),
        .is_scalar_logical(verbose)
    )

    repos <- repositories(version, binary_base_url)
    install.packages(pkgs, repos = repos, lib = lib, ..., verbose = verbose)
}

#' @rdname localize
#'
#' @description `repositories()`: repositories to search for binary
#'     (if available), Bioconductor, and CRAN packages.
#'
#' @details `repositories()` prepends an additional repository URI to
#'     `BiocManager::repositories()`. The URI is formed by
#'     concatenating `binary_base_url`, the environment variables
#'     `TERRA_R_PLATFORM` and the 'major' and 'minor' components of
#'     `TERRA_R_PLATFORM_BINARY_VERSION` and
#'     `BiocManager::version()`. The URI is only prepended if a
#'     CRAN-style repostiory exists at that location, with binary
#'     package tar.gz content described by `src/contrib/PACKAGES.gz`.
#'
#' @return `repositories()`: character() of binary (if available),
#'     Bioconductor, and CRAN repositories.
#'
#' @examples
#' repositories()
#'
#' @export
repositories <-
    function(version = BiocManager::version(),
        binary_base_url = "https://storage.googleapis.com")
{
    stopifnot(
        .is_scalar_character(version) || is.package_version(version),
        .is_scalar_character(binary_base_url)
    )

    binary_repos <- NULL
    bioconductor_version <- package_version(version)
    platform <- Sys.getenv("TERRA_R_PLATFORM", NA)
    platform_version_string <- Sys.getenv("TERRA_R_PLATFORM_BINARY_VERSION", NA)
    if (!is.na(platform) && !is.na(platform_version_string)) {
        ## binary_repos = https://storage.googleapis.com/terra-jupyter-r/0.99"
        ## binary_repos = https://storage.googleapis.com/terra-rstudio-bioconductor/0.99"
        ## CRAN-style exetension: src/contrib/PACKAGES.gz
        platform_version <- package_version(platform_version_string)
        binary_repos0 <- paste0(
            binary_base_url, "/",
            platform, "/",
            platform_version$major, ".", platform_version$minor, "/",
            bioconductor_version$major, ".", bioconductor_version$minor

        )
        ## validate binary_repos is available
        packages <- paste0(contrib.url(binary_repos0), "/PACKAGES.gz")
        url <- url(packages)
        binary_repos <- tryCatch({
            suppressWarnings(open(url, "rb"))
            close(url)
            binary_repos0
        }, error = function(...) {
            close(url)
            NULL
        })
    }

    c(binary_repos, BiocManager::repositories())
}

#' @rdname localize
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
