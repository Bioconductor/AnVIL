test_that("Dockstore API reference version is constant", {
    api_reference_url <- "https://dockstore.org/openapi.yaml"
    api_reference_headers <- NULL
    api_reference_version <- AnVIL:::.DOCKSTORE_API_REFERENCE_VERSION
    api_file <- .service_get_api_file(
        reference_url = api_reference_url,
        reference_headers = api_reference_headers
    )

    expect_true(
        .service_validate_version(
            api_reference_url, api_reference_version,
            api_reference_headers, api_file
        )
    )
})
