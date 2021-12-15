BINARY_BASE_URL <- "https://bioconductor.org/packages/%s/container-binaries"

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
    ## are we running on a docker container?
    bioconductor_docker_version <- Sys.getenv("BIOCONDUCTOR_DOCKER_VERSION")
    if (!nzchar(bioconductor_docker_version))
        return(character())

    ## is the docker container configured correctly?
    bioconductor_version <- package_version(version)
    docker_version <- package_version(bioconductor_docker_version)
    test <-
        (bioconductor_version$major == docker_version$major) &
        (bioconductor_version$minor == docker_version$minor)
    if (!test) {
        return(character())
    }

    ## does the binary repository exist?
    binary_repos0 <- sprintf(binary_base_url, bioconductor_version)
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

.repository_stats_package_format <-
    function(x)
{
    msg <- paste(sort(x), collapse = " ")
    paste(strwrap(msg, indent = 2L, exdent = 2L), collaspe = "\n")
}

#' @importFrom utils available.packages
repository_stats <-
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
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

    cat(
        "Bioconductor software packages: ", nrow(db_bioc), "\n",
        "Binary packages: ", nrow(db_binary), "\n",
        "Binary software packages: ", length(found_binaries), "\n",
        "Missing binary software packages: ", length(missing_binaries), "\n",
        .repository_stats_package_format(missing_binaries),
        "Out-of-date binary software packages: ", n_out_of_date_binaries, "\n",
        .repository_stats_package_format(out_of_date_binaries),
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
