context("gsutil")

test_that("'.gsutil_is_uri()' works", {
    expect_identical(.gsutil_is_uri(character()), logical(0))
    expect_identical(.gsutil_is_uri("gs://bucket"), TRUE)
    expect_identical(.gsutil_is_uri(c("gs://bucket", "/path")), c(TRUE, FALSE))
    expect_identical(.gsutil_is_uri(1), FALSE)
})

test_that(".gsutil_sh_quote()' works", {
    home_dir <- "~"
    bucket <- "gs://foo"
    expect_identical(.gsutil_sh_quote(home_dir), shQuote(path.expand(home_dir)))
    expect_identical(.gsutil_sh_quote(bucket), shQuote(bucket))

    non_existent_dir <- tempfile(tmpdir="~")
    expect_warning(.gsutil_sh_quote(non_existent_dir))
    suppressWarnings({
        expect_identical(
            .gsutil_sh_quote(non_existent_dir),
            shQuote(normalizePath(non_existent_dir))
        )
    })

    all <- c(home_dir, bucket, non_existent_dir)
    expect_warning(.gsutil_sh_quote(all))
    suppressWarnings({
        expect_identical(
            .gsutil_sh_quote(all),
            shQuote(c(
                normalizePath(home_dir),
                bucket,
                normalizePath(non_existent_dir)
            ))
        )
    })
})
