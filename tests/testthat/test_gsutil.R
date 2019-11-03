context("gsutil")

test_that("'.gsutil_is_uri()' works", {
    expect_identical(.gsutil_is_uri(character()), logical(0))
    expect_identical(.gsutil_is_uri("gs://bucket"), TRUE)
    expect_identical(.gsutil_is_uri(c("gs://bucket", "/path")), c(TRUE, FALSE))
    expect_identical(.gsutil_is_uri(1), FALSE)
})
