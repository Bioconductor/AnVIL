BINARY_BASE_URL <- "https://bioconductor.org/packages/%s/container-binaries/%s"

#' @rdname install
#'
#' @title Discover and install binary packages for fast installation
#'
#' @description `install()`: install R / Bioconductor packages, using
#'     fast pre-built 'binary' libraries if available.
#'
#' @param pkgs `character()` packages to install from binary repository.
#'
#' @param ... additional arguments, passed to `BiocManager::install()`.
#'
#' @param version `character(1)` or `package_version` Bioconductor
#'     version, e.g., "3.12".
#'
#' @param binary_base_url `character(1)` host and base path for binary
#'     package 'CRAN-style' repository; not usually required by the
#'     end-user.
#'
#' @return `install()`: return value of `BiocManager::install()`.
#'
#' @examples
#' \dontrun{install(c('BiocParallel', 'BiocGenerics'))}
#'
#' @export
install <-
    function(
        pkgs = character(), ...,
        version = BiocManager::version(), binary_base_url = BINARY_BASE_URL
    )
{
    stopifnot(
        .is_character(pkgs),
        .is_scalar_character(version) || is.package_version(version),
        .is_scalar_character(binary_base_url)
    )
    if ("site_repository" %in% names(list(...))) {
        stop("'site_repository=' cannot be used with AnVIL::install()")
    }

    site_repository <- repository(version, binary_base_url)
    BiocManager::install(
                     pkgs = pkgs,
                     ...,
                     site_repository = site_repository,
                     version = version
                 )
}

#' @rdname install
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
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
    stopifnot(
        .is_scalar_character(version) || is.package_version(version),
        .is_scalar_character(binary_base_url)
    )

    repositories <- BiocManager::repositories()
    binary_repository <- repository(version, binary_base_url)
    if (length(binary_repository))
        repositories <- c(BiocBinaries = binary_repository, repositories)

    repositories
}

## is the docker container configured correctly?
.test_container_bioc_versions <- function(version, container_version) {
    bioconductor_version <- package_version(version)
    docker_version <- package_version(container_version)
    (bioconductor_version$major == docker_version$major) &
        (bioconductor_version$minor == docker_version$minor)
}

## are we running on a docker container?
.platform_bioc_docker_version <- function() {
    platform <- "bioconductor_docker"
    bioconductor_docker_version <- Sys.getenv("BIOCONDUCTOR_DOCKER_VERSION")
    if (!nzchar(bioconductor_docker_version)) {
        platform <- Sys.getenv("TERRA_R_PLATFORM")
        bioconductor_docker_version <-
            Sys.getenv("TERRA_R_PLATFORM_BINARY_VERSION")
    }
    c(platform = platform,
        bioconductor_docker_version = bioconductor_docker_version)
}


#' @rdname install
#'
#' @aliases BINARY_BASE_URL
#'
#' @description `repository()`: the location of the repository of
#'     binary packages for fast installation, if available.
#'
#' @details The unexported URL to the base repository is available
#'     with `AnVIL:::BINARY_BASE_URL`.
#'
#' @return `repository()`: character(1) location of binary repository,
#'     if available, or character(0) if not.
#'
#' @examples
#' repository()
#'
#' @importFrom utils contrib.url
#'
#' @export
repository <-
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
    platform_docker <- .platform_bioc_docker_version()
    
    bioconductor_docker_version <-
        platform_docker[["bioconductor_docker_version"]]
    platform <- platform_docker[["platform"]]
    bioconductor_version <- package_version(version)
    
    if (!nzchar(bioconductor_docker_version) ||
        !.test_container_bioc_versions(
            bioconductor_version, bioconductor_docker_version
        )
    )
        return(character())

    ## does the binary repository exist?
    binary_repos0 <- sprintf(
        binary_base_url,
        bioconductor_version,
        platform
    )
    packages <- paste0(contrib.url(binary_repos0), "/PACKAGES.gz")
    url <- url(packages)
    binary_repository <- tryCatch({
        suppressWarnings(open(url, "rb"))
        close(url)
        binary_repos0
    }, error = function(...) {
        close(url)
        character()
    })

    binary_repository
}

#' @importFrom utils available.packages
repository_stats <-
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
    platform_docker <- .platform_bioc_docker_version()
    container <- platform_docker[["platform"]]
    bioc_repository <- suppressMessages({
        BiocManager::repositories()[["BioCsoft"]]
    })
    binary_repository <- repository(version, binary_base_url)
    db_bioc <- available.packages(repos = bioc_repository)
    db_binary <- available.packages(repos = binary_repository)

    missing_binaries <- setdiff(rownames(db_bioc), rownames(db_binary))
    found_binaries <- intersect(rownames(db_bioc), rownames(db_binary))

    bioc_versions <- package_version(db_bioc[found_binaries, "Version"])
    binary_versions <- package_version(db_binary[found_binaries, "Version"])
    binary_out_of_date <- bioc_versions > binary_versions
    n_out_of_date_binaries <- sum(binary_out_of_date)
    out_of_date_binaries <- found_binaries[binary_out_of_date]

    result <- list(
        container = container,
        bioconductor_version = version,
        bioconductor_binary_repository = binary_repository,
        repository_exists = length(binary_repository) > 0L,
        n_software_packages = nrow(db_bioc),
        n_binary_packages = nrow(db_binary),
        n_binary_software_packages = length(found_binaries),
        missing_binaries = missing_binaries,
        out_of_date_binaries = out_of_date_binaries
    )
    class(result) <- c("repository_stats", class(result))
    result
}

.repository_stats_package_format <-
    function(x)
{
    msg <- paste(sort(x), collapse = " ")
    paste(strwrap(msg, indent = 2L, exdent = 2L), collaspe = "\n")
}

#' @export
print.repository_stats <-
    function(x, ...)
{
    cat(
        "Container: ", x$container, "\n",
        "Bioconductor version: ", as.character(x$bioconductor_version), "\n",
        "Bioconductor binary repos: ", x$bioconductor_binary_repository, "\n",
        "Bioconductor software packages: ", x$n_software_packages, "\n",
        "Binary packages: ", x$n_binary_packages, "\n",
        "Binary software packages: ", x$n_binary_software_packages, "\n",
        "Missing binary software packages: ", length(x$missing_binaries), "\n",
        if (x$repository_exists)
            .repository_stats_package_format(x$missing_binaries),
        "Out-of-date binary software packages: ",
            length(x$out_of_date_binaries), "\n",
        if (x$repository_exists)
            .repository_stats_package_format(x$out_of_date_binaries),
        sep = ""
    )
}

#' @rdname install
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
