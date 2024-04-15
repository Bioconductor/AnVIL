test_that("repositories() works", {
    ## unknown repository; return BiocManager::repositories()
    envvars <- c(BIOCONDUCTOR_DOCKER_VERSION = "3.0")
    expect_warning(object <- withr::with_envvar(envvars, repositories("3.0")))
    ## need to suppress deprecation warnings
    suppressWarnings({
        expect_identical(
            object,
            withr::with_envvar(envvars, BiocManager::repositories())
        )

        ## existing repository
        envvars <- c(BIOCONDUCTOR_DOCKER_VERSION = "3.14.0")
        object <- withr::with_envvar(envvars, repositories("3.14"))
        expected <- c(
            BiocBinaries = paste0(
                "https://bioconductor.org/packages/3.14/container-binaries/",
                "bioconductor_docker"
            ),
            withr::with_envvar(envvars, BiocManager::repositories())
        )
        expect_identical(object, expected)
    })
})
