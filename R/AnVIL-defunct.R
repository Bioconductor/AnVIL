BINARY_BASE_URL <- "https://bioconductor.org/packages/%s/container-binaries/%s"

#' @name AnVIL-defunct
#'
#' @title Defunct AnVIL functionality
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
#' @export
repository_stats <-
    function(
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL)
{
    .Defunct("BiocPkgTools::repositoryStats", package = "AnVIL")
}

#' @describeIn AnVIL-defunct Print a summary of package
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
    .Defunct("BiocPkgTools:::print.repositoryStats", package = "AnVIL")
}
