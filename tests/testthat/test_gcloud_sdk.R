context("gcloud_sdk")

test_that(".gcloud_sdk_find_binary() works", {
    with_envvar <- withr::with_envvar

    with_envvar(c(GCLOUD_SDK_PATH=tempdir()), {
        object <- .gcloud_sdk_find_binary("gsutil")
        expect_identical(object, file.path(tempdir(), "bin", "gsutil"))
    })
})

