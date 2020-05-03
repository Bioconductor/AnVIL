test_that("Services are current", {
    expect_silent(Terra())
    expect_silent(Leonardo())
    expect_silent(Dockstore())
})
