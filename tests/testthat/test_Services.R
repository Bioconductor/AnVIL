test_that("Services are current", {
    skip_if(!gcloud_exists())
    ## suppressWarnings() is needed to suppress the warnings
    ## about the deprecated functions (e.g., gcloud_account())
    expect_silent(
        suppressWarnings(Terra())
    )
    expect_silent(
        suppressWarnings(Leonardo())
    )
    expect_silent(
        suppressWarnings(Rawls())
    )
    expect_silent(Dockstore())
})
