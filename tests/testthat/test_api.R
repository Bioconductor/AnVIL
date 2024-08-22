test_that("positional matching works for body arguments", {
    skip_if(!AnVILGCP::gcloud_exists())

    ## two arguments for URL, one for BODY
    fun <- Terra()$flexibleImportEntities

    with_mock(
        `httr::POST` = function(..., body) identical(names(body), "entities"),
        ## named...
        expect_true(fun("A", "B", .__body__ = list(entities = "C"))),
        expect_true(fun("A", "B", entities = "C")),
        ## positional
        expect_true(fun("A", "B", .__body__ = list("C"))),
        expect_true(fun("A", "B", , , "C"))
    )

})

## Check that the API calls used by AnVIL are consistent with the API
## in the YAML. Requires manual investigation of any removed or
## updated_args_in_use functions.
##
## Use functionality in R/api.R:.api_test_write() to record the
## current interface; .api_test_check() to compare the current and
## previously recorded versions.

test_that("Interfaces are current", {
    skip_if(!AnVILGCP::gcloud_exists())

    service_status <- .api_test_check(Terra(), "Terra")
    expect_identical(service_status$removed_in_use, character())
    expect_identical(service_status$updated_in_use, character())

    service_status <- .api_test_check(Rawls(), "Rawls")
    expect_identical(service_status$removed_in_use, character())
    expect_identical(service_status$updated_in_use, character())

    service_status <- .api_test_check(Leonardo(), "Leonardo")
    expect_identical(service_status$removed_in_use, character())
    expect_identical(service_status$updated_in_use, character())
})
