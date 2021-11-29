test_that("repositories() works", {
    ## unknown repository; return BiocManager::repositories()
    envvars <- c(BIOCONDUCTOR_DOCKER_VERSION = "3.0")
    object <- withr::with_envvar(envvars, repositories("3.0"))
    expect_identical(object, BiocManager::repositories())

    ## existing repository
    envvars <- c(BIOCONDUCTOR_DOCKER_VERSION = "3.13.0")
    object <- withr::with_envvar(envvars, repositories("3.13"))
    expected <- c(
        BiocBinaries =
            "https://bioconductor.org/packages/3.13/container-binaries",
        BiocManager::repositories()
    )
    expect_identical(object, expected)
})
