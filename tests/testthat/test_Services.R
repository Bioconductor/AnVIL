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

test_that(".service_validate_digest checks md5 and sha256 digests", {
    api_file <- tempfile(fileext = ".json")
    writeLines('{"info": {"version": "1.0"}}', api_file)
    on.exit(unlink(api_file))
    md5 <- unname(tools::md5sum(api_file))
    sha256 <- digest::digest(api_file, algo = "sha256", file = TRUE)
    url <- paste0("https://example.org/", basename(api_file))

    expect_null(
        .service_validate_digest(url, character(), NULL, api_file, "md5")
    )
    expect_true(
        .service_validate_digest(url, md5, NULL, api_file, algo = "md5")
    )
    expect_true(
        .service_validate_digest(url, sha256, NULL, api_file, algo = "sha256")
    )
    ## mismatches warn once per url and algorithm
    bad_url <- paste0(url, "?mismatch")
    expect_warning(
        res <- .service_validate_digest(
            bad_url, sha256, NULL, api_file, algo = "md5"
        ),
        "observed md5"
    )
    expect_false(res)
    expect_silent(
        .service_validate_digest(bad_url, sha256, NULL, api_file, algo = "md5")
    )
    expect_warning(
        .service_validate_digest(bad_url, md5, NULL, api_file, algo = "sha256"),
        "observed sha256"
    )
})

test_that("Service validates api_reference_md5sum and api_reference_sha256", {
    api_file <- tempfile(fileext = ".json")
    writeLines(
        '{"swagger": "2.0", "info": {"version": "1.0"}, "paths": {}}',
        api_file
    )
    on.exit(unlink(api_file))
    md5 <- unname(tools::md5sum(api_file))
    sha256 <- digest::digest(api_file, algo = "sha256", file = TRUE)
    service <- function(...)
        Service(
            "test", host = "example.org", authenticate = FALSE,
            api_url = api_file,
            api_reference_url = paste0("file://", api_file), ...
        )

    expect_s4_class(service(api_reference_md5sum = md5), "Service")
    expect_silent(service(api_reference_md5sum = md5))
    expect_silent(service(api_reference_sha256 = sha256))
    expect_silent(
        service(api_reference_md5sum = md5, api_reference_sha256 = sha256)
    )
    ## backward compatibility: sha256 digest supplied as md5sum
    expect_silent(service(api_reference_md5sum = sha256))
})

test_that("Service() skips the API reference when nothing is validated", {
    api_url <- tempfile(fileext = ".json")
    writeLines(
        '{"swagger": "2.0", "info": {"title": "t", "version": "1"},
         "paths": {}}',
        api_url
    )
    calls <- 0L
    local_mocked_bindings(
        .service_get_api_file = function(reference_url, reference_headers) {
            calls <<- calls + 1L
            api_url
        }
    )

    service <- Service(
        "my_service", host = "my.api.org", authenticate = FALSE,
        api_url = api_url
    )
    expect_s4_class(service, "Service")
    expect_identical(calls, 0L)

    sha256 <- digest::digest(api_url, algo = "sha256", file = TRUE)
    expect_silent(
        Service(
            "my_service", host = "my.api.org", authenticate = FALSE,
            api_url = api_url, api_reference_sha256 = sha256
        )
    )
    expect_identical(calls, 1L)

    expect_warning(
        Service(
            "my_service", host = "my.api.org", authenticate = FALSE,
            api_url = api_url,
            api_reference_url = "https://my.api.org/sha256-mismatch",
            api_reference_sha256 = strrep("0", 64L)
        ),
        "service version differs from validated version"
    )
    expect_identical(calls, 2L)

    expect_silent(
        Service(
            "my_service", host = "my.api.org", authenticate = FALSE,
            api_url = api_url, api_reference_version = "1"
        )
    )
    expect_identical(calls, 3L)
})
