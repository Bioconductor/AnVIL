context("gcloud_sdk")

test_that(".gcloud_sdk_find_binary() works", {
    with_envvar <- withr::with_envvar
    sdk <- tempfile()
    dir.create(file.path(sdk, "bin", "gsutil"), recursive = TRUE)
    sdk <- normalizePath(sdk)
    expected <- normalizePath(file.path(sdk, "bin", "gsutil"))

    with_envvar(c(GCLOUD_SDK_PATH=sdk), {
        object <- .gcloud_sdk_find_binary("gsutil")
        expect_identical(object, expected)
    })
})

test_that("gcloud_project() returns correctly when config unset", {
    skip_if(!gcloud_exists())
    with_envvar <- withr::with_envvar
    with_envvar(c(CLOUDSDK_ACTIVE_CONFIG_NAME="__UNDEFINED__"), {
        suppressMessages({
            object <- gcloud_project()
        })
        expect_identical(object, "(unset)")
    })
})
