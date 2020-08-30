context("gsutil")

test_that("'.gsutil_is_uri()' works", {
    expect_identical(.gsutil_is_uri(character()), logical(0))
    expect_identical(.gsutil_is_uri("gs://bucket"), TRUE)
    expect_identical(.gsutil_is_uri(c("gs://bucket", "/path")), c(TRUE, FALSE))
    expect_identical(.gsutil_is_uri(1), FALSE)
})

## Notes: gs://test-gcsconnection is being used as a permanently
## hosted bucket to test functionality of key components in the AnVIL
## package.
test_that("'gsutil_requesterpays()' functionality works", {
    reqpays_true_bucket <- "gs://test-gcsconnection"
    reqpays_false_bucket <- "gs://genomics-public-data/"
    expect_true(gsutil_requesterpays(reqpays_true_bucket))
    expect_false(gsutil_requesterpays(reqpays_false_bucket))
})

## Also test GCSConnection functionality
test_that("'gsutil_ls()' works", {
    bucket <- "gs://genomics-public-data/"
    res <- gsutil_ls(bucket)
    expect_is(res, "FolderClass")
    expect_is(res$README, "FileClass")

    expect_identical(res$README$uri, "gs://genomics-public-data/README")
    expect_identical(.gsutil_is_uri(res$README$uri), TRUE)
})


## Test gsutil_cp
test_that("'gsutil_cp()' works", {
    file_in_bucket <- "gs://genomics-public-data/README"
    
    temp_path <- tempdir()

    if (gcloud_exists()) {
        gsutil_cp(file_in_bucket, temp_path)
        expect_true(file.exists(file.path(temp_path, "README")))
        ## clean up after copy
        unlink(temp_path, recursive=TRUE)
        expect_false(file.exists(temp_path))
    }
})
