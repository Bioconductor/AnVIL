## are we running on a docker container?
.repository_container_version <-
    function()
{
    container_version <- Sys.getenv("BIOCONDUCTOR_DOCKER_VERSION")
    if (nzchar(container_version)) {
        platform <- "bioconductor_docker"
    } else {
        platform <- Sys.getenv("TERRA_R_PLATFORM")
        container_version <- Sys.getenv("TERRA_R_PLATFORM_BINARY_VERSION")
    }

    list(platform = platform, container_version = container_version)
}

#' @name AnVIL-deprecated
#'
#' @title Deprecated AnVIL functionality
#'
#' @aliases print.repository_stats
#'
#' @description `repository_stats():` summarize binary packages
#'     compatible with the Bioconductor or Terra container in use.
#'
#' @param version `character(1)` or `package_version` Bioconductor
#'     version, e.g., "3.12".
#'
#' @param binary_base_url `character(1)` host and base path for binary
#'     package 'CRAN-style' repository; not usually required by the
#'     end-user.
#'
#' @return `repository_stats()` returns a list of class
#'     `repository_stats` with the following fields:
#'
#' * container: character(1) container label, e.g.,
#' \code{bioconductor_docker}, or NA if not evaluated on a supported
#' container
#'
#' * bioconductor_version: \code{package_version} the
#' Bioconductor version provided by the user.
#'
#' * repository_exists: logical(1) TRUE if a binary repository
#' exists for the container and Bioconductor_Version version.
#'
#' * bioconductor_binary_repository: character(1) repository
#' location, if available, or NA if the repository does not exist.
#'
#' * n_software_packages: integer(1) number of software packages
#' in the Bioconductor source repository.
#'
#' * n_binary_packages: integer(1) number of binary packages
#' available. When a binary repository exists, this number is likely
#' to be larger than the number of source software packages, because
#' it includes the binary version of the source software packages, as
#' well as the (possibly CRAN) dependencies of the binary packages
#'
#' * n_binary_software_packages: integer(1) number of binary
#' packages derived from Bioconductor source packages. This number is
#' less than or equal to \code{n_software_packages}.
#'
#' * missing_binaries: integer(1) the number of Bioconductor
#' source software packages that are not present in the binary
#' repository.
#'
#' * out_of_date_binaries: integer(1) the number of Bioconductor
#' source software packages that are newer than their binary
#' counterpart. A newer source software package
#' might occur when the main Bioconductor build system has
#' updated a package after the most recent run of the binary
#' build system.
#'
#' @importFrom utils available.packages
#'
#' @examples
#' stats <- repository_stats() # obtain statistics
#' stats                       # display a summary
#' stats$container             # access an element for further computation
#'
#' @export
repository_stats <-
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
    .Deprecated("BiocPkgTools::repositoryStats()", package = "AnVIL")
    platform_docker <- .repository_container_version()
    container <- platform_docker$platform
    bioc_repository <- suppressMessages({
        BiocManager::repositories()[["BioCsoft"]]
    })
    binary_repository <- BiocManager::containerRepository(version)
    db_bioc <- available.packages(repos = bioc_repository)
    if (length(binary_repository)) {
        db_binary <- available.packages(repos = binary_repository)
        packages <- paste0(contrib.url(binary_repository), "/PACKAGES")
        response <- HEAD(packages)
        last_modified <- headers(response)$`last-modified`
        PACKAGES_mtime <-
            format(strptime(
                last_modified, "%a, %d %b %Y %H:%M", tz = "UTC"
            ), usetz = TRUE)
    } else {
        db_binary <- db_bioc[NULL,]
        PACKAGES_mtime <- NA_character_
    }

    missing_binaries <- setdiff(rownames(db_bioc), rownames(db_binary))
    found_binaries <- intersect(rownames(db_bioc), rownames(db_binary))

    bioc_versions <- package_version(db_bioc[found_binaries, "Version"])
    binary_versions <- package_version(db_binary[found_binaries, "Version"])
    binary_out_of_date <- bioc_versions > binary_versions
    n_out_of_date_binaries <- sum(binary_out_of_date)
    out_of_date_binaries <- found_binaries[binary_out_of_date]

    query_timestamp = format(
        Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC", usetz = TRUE
    )

    result <- list(
        container = if (nzchar(container)) container else NA_character_,
        bioconductor_version = version,
        bioconductor_binary_repository =
            if (length(binary_repository)) binary_repository else NA_character_,
        PACKAGES_mtime = PACKAGES_mtime,
        query_timestamp = query_timestamp,
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

#' @describeIn AnVIL-deprecated Print a summary of package
#'     availability in binary repositories.
#'
#' @param x the object returned by `repository_stats()`.
#'
#' @param ... additional arguments (not used).
#'
#' @export
print.repository_stats <-
    function(x, ...)
{
    bioconductor_binary_repository <- ifelse(
        is.na(x$bioconductor_binary_repository),
        paste(" ", x$bioconductor_binary_repository),
        paste("\n  ", x$bioconductor_binary_repository)
    )
    cat(
        "Container: ", x$container, "\n",
        "Bioconductor version: ", as.character(x$bioconductor_version), "\n",
        "Bioconductor binary repos:", bioconductor_binary_repository, "\n",
        "PACKAGES timestamp: ", x$PACKAGES_mtime, "\n",
        "Query timestamp: ", x$query_timestamp, "\n",
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
