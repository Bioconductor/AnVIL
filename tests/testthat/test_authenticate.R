test_that(".authenticate_get_access works with mocked keyring", {
    service <- "test_service"
    content <- '{"token": "test_token"}'

    # Mocking in AnVIL package since it imports these
    with_mocked_bindings(
        key_list = function(service = NULL, keyring = NULL) {
            data.frame(username = "test_service", stringsAsFactors = FALSE)
        },
        key_get = function(service, username, keyring = NULL) content,
        {
            access <- .authenticate_get_access(service)
            expect_equal(access$token, "test_token")
        },
        .package = "AnVIL"
    )
})

test_that(".authenticate_get_access falls back to system.file with warning", {
    service <- "non_existent_service"

    with_mocked_bindings(
        key_list = function(service = NULL, keyring = NULL) {
            data.frame(username = character(), stringsAsFactors = FALSE)
        },
        {
            expect_warning(
                expect_null(.authenticate_get_access(service)),
                NA
            )
        },
        .package = "AnVIL"
    )
})

test_that("anvil_set_auth_json validates file and calls key_set_with_value", {
    service <- "test_service"
    path <- tempfile()
    writeLines('{"token": "test_token"}', path)
    on.exit(unlink(path))

    called_with <- list()
    with_mocked_bindings(
        key_set_with_value =
            function(service, username, password, keyring = NULL) {
                called_with <<- list(
                    service = service, username = username, password = password
                )
            },
            {
                anvil_set_auth_json(service, path)
                expect_equal(called_with$service, "AnVIL")
                expect_equal(called_with$username, service)
                expect_match(called_with$password, "test_token")
            },
            .package = "AnVIL"
    )
})
