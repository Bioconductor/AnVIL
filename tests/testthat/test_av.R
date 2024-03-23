context("av")

test_that(".avtable_import_write_dataset() works with NA", {
    tbl0 <- tibble(
        issue75 = letters[1:5],
        x = c("a", "", NA, "NA", "d"),
        i = c(1:3, NA, 4L),
        r = as.numeric(i)
    )

    na <- ""
    destination <- .avtable_import_write_dataset(tbl0, na)
    result <- read.delim(destination, na.strings = na, sep = "\t")
    expect_identical(result$x, c("a", NA, NA, "NA", "d"))

    na <- NA_character_
    destination <- .avtable_import_write_dataset(tbl0, na)
    result <- read.delim(destination, na.strings = na, sep = "\t")
    expect_identical(result$x, c("a", "", NA, NA, "d"))

    na <- "__MISSING__"
    destination <- .avtable_import_write_dataset(tbl0, na)
    result <- read.delim(destination, na.strings = na, sep = "\t")
    expect_identical(result$x, tbl0$x)
})

test_that(".avtable_na() works", {

    ## .avtable_na() acts when flatten() has converted "" to NA_character_
    tbl0 <- tibble(
        issue75 = letters[1:4],
        x = c("a", NA, "NA", "d"),
        i = c(1:2, NA, 3L),
        r = as.numeric(i)
    )

    tbl1 <- mutate(tbl0, across(where(is.character), .avtable_na("")))
    expect_identical(tbl1$x, c("a", NA, "NA", "d"))

    tbl1 <- mutate(tbl0, across(where(is.character), .avtable_na("NA")))
    expect_identical(tbl1$x, c("a", "", NA, "d"))

    tbl1 <- mutate(tbl0, across(where(is.character), .avtable_na(c("", "NA"))))
    expect_identical(tbl1$x, c("a", NA, NA, "d"))

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
