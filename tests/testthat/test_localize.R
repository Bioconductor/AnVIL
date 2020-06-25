test_that("repositories() works", {
    ## unknown repository; return BiocManager::repositories()
    envvars <- c(
        TERRA_R_PLATFORM = "MOCK", TERRA_R_PLATFORM_BINARY_VERSION = "0.99.1"
    )
    object <- withr::with_envvar(envvars, repositories("3.10"))
    expect_identical(object, BiocManager::repositories())

    envvars <- c(
        TERRA_R_PLATFORM = "anvil-rstudio-bioconductor",
        TERRA_R_PLATFORM_BINARY_VERSION = "0.99.1"
    )
    object <- withr::with_envvar(envvars, repositories("3.10"))
    expected <- c(
        "https://storage.googleapis.com/anvil-rstudio-bioconductor/0.99/3.10",
        BiocManager::repositories()
    )
    expect_identical(object, expected)
})
