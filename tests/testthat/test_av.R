context("av")

test_that(".avtable_import_set_entity() works", {
    .data <- data.frame(x=1:3, y=1:3, z_id=1:3)
    
    expect_identical(
        names(.avtable_import_set_entity(.data, "x")),
        c("entity:x_id", "y", "z_id")
    )

    expect_identical(
        names(.avtable_import_set_entity(.data, "y")),
        c("entity:y_id", "x", "z_id")
    )

    expect_identical(
        names(.avtable_import_set_entity(.data, "z_id")),
        c("entity:z_id", "x", "y")
    )

    ## construct ids from unknown column
    observed <- .avtable_import_set_entity(.data, "w")
    expect_identical(names(observed), c("entity:w_id", "x", "y", "z_id"))
    expect_identical(observed[["entity:w_id"]], 1:3)

    observed <- .avtable_import_set_entity(.data, "w_id")
    expect_identical(names(observed), c("entity:w_id", "x", "y", "z_id"))
    expect_identical(observed[["entity:w_id"]], 1:3)

    ## errors: duplicate or NA entities
    .data <- data.frame(x=c(1, 1), y=1)
    expect_error(
        .avtable_import_set_entity(.data, "x"),
        "!anyDuplicated\\(.data\\[\\[entity\\]\\]\\) is not TRUE"
    )

    .data <- data.frame(x=c(1, NA, 2), y=1)
    expect_error(
        .avtable_import_set_entity(.data, "x"),
        "!anyNA\\(.data\\[\\[entity\\]\\]\\) is not TRUE"
    )
})

test_that(".avbucket_path() trims arguments correctly", {
    expect_error(
        .avbucket_path(),
        'argument "bucket" is missing, with no default'
    )
    expect_error(
        .avbucket_path('foo'),
        '.gsutil_is_uri\\(bucket\\) is not TRUE'
    )
    expect_identical("gs://foo", .avbucket_path("gs://foo"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "bar"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "/bar"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "bar/"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "/bar/"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo/", "/bar"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo/", "bar/"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo/", "/bar/"))
    expect_identical(
        paste0("gs://foo/", c("bar", "baz")),
        .avbucket_path("gs://foo", c("bar", "baz"))
    )
    expect_identical(
        "gs://foo/bar/baz",
        .avbucket_path("gs://foo", "bar", "baz")
    )
    expect_identical(
        paste0("gs://foo/bar/", c("baz", "bing")),
        .avbucket_path("gs://foo", "bar", c("baz", "bing"))
    )
})
