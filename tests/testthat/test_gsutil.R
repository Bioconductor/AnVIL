context("gsutil")

test_that(".gsutil_find_binary() works", {
    with_envvar <- withr::with_envvar

    with_envvar(c(GSUTIL_BINARY_PATH=tempdir()), {
        object <- .gsutil_find_binary("gsutil")
        expect_identical(object, normalizePath(tempdir()))
    })

    with_envvar(c(GSUTIL_BINARY_PATH=tempdir()), {
        object <- .gsutil_find_binary("gcloud")
        expect_identical(object, normalizePath(tempdir()))
    })

    with_envvar(c(GCLOUD_INSTALL_PATH=tempdir()), {
        object <- .gsutil_find_binary("gsutil")
        expect_identical(
            object,
            file.path(normalizePath(tempdir()), "bin", "gsutil")
        )
    })

    with_envvar(c(GCLOUD_INSTALL_PATH=tempdir()), {
        object <- .gsutil_find_binary("gcloud")
        expect_identical(
            object,
            file.path(normalizePath(tempdir()), "bin", "gcloud")
        )
    })
})

test_that("'.gsutil_is_uri()' works", {
    expect_identical(.gsutil_is_uri(character()), logical(0))
    expect_identical(.gsutil_is_uri("gs://bucket"), TRUE)
    expect_identical(.gsutil_is_uri(c("gs://bucket", "/path")), c(TRUE, FALSE))
    expect_error(.gsutil_is_uri(1:5))
})
