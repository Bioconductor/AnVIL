test_that("Services are current", {
    skip_if(!GCPtools::gcloud_exists())
    expect_silent(Terra())
    expect_silent(Leonardo())
    expect_silent(Rawls())
    expect_silent(Dockstore())
})

test_that("host is captured in Service", {
    api_reference_url <- "https://dockstore.org/openapi.yaml"
    api_reference_version <- AnVIL:::.DOCKSTORE_API_REFERENCE_VERSION
    .host <- function(x) x@host
    myHost <- "dockstore.org"

    .MyService <- setClass("MyService", contains = "Service")
    MyService <- function() {
        .MyService(
            Service(
                "dockstore",
                host = myHost,
                api_reference_version = api_reference_version,
                authenticate = FALSE,
                api_reference_url = "https://dockstore.org/api/openapi.yaml",
            )
        )
    }

    expect_identical(
        .host(MyService()), myHost
    )
})

test_that("Dockstore API reference version is constant", {
    api_reference_url <- "https://dockstore.org/openapi.yaml"
    # .service_read_version(api_reference_url)
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

test_that("Rawls API reference version is constant", {
    api_reference_url <-
        "https://rawls.dsde-prod.broadinstitute.org/api-docs.yaml"
    # .service_read_version(api_reference_url)
    api_reference_headers <- NULL
    api_reference_version <- AnVIL:::.RAWLS_API_REFERENCE_VERSION
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

test_that("Leonardo API reference version is constant", {
    api_reference_url <-
        "https://leonardo.dsde-prod.broadinstitute.org/api-docs.yaml"
    # .service_read_version(api_reference_url)
    api_reference_headers <- NULL
    api_reference_version <- AnVIL:::.LEONARDO_API_REFERENCE_VERSION
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

test_that("Terra API reference version is constant", {
    api_reference_url <- "https://api.firecloud.org/api-docs.yaml"
    # .service_read_version(api_reference_url)
    api_reference_headers <- NULL
    api_reference_version <- AnVIL:::.TERRA_API_REFERENCE_VERSION
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
