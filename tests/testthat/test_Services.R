test_that("Services are current", {
    skip_if(!gcloud_exists())
    expect_silent(Terra())
    expect_silent(Leonardo())
    expect_silent(Dockstore())
})
