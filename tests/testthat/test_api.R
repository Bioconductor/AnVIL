test_that("positional matching works for body arguments", {
    skip_if(!gcloud_exists())
    
    ## two arguments for URL, one for BODY
    fun <- Terra()$flexibleImportEntities

    with_mock(
        `httr::POST` = function(..., body) identical(names(body), "entities"),
        ## named...
        expect_true(fun("A", "B", .__body__ = list(entities = "C"))),
        expect_true(fun("A", "B", entities = "C")),
        ## positional
        expect_true(fun("A", "B", .__body__ = list("C"))),
        expect_true(fun("A", "B", "C"))
    )

})

test_that("'accept' defaults to */*", {
    op_def <- list(produces = NULL)
    expect_identical(.api_get_accept(op_def), httr::accept("*/*"))

    produces <- "application/json"
    op_def <- list(produces = produces)
    expect_identical(.api_get_accept(op_def), httr::accept(produces))
})
