context("test_sync")

test_that(".gcs_pathify works", {
    expect_identical(.gcs_pathify("foo"), "gs://foo")
    expect_identical(.gcs_pathify("gs://foo"), "gs://foo")

    obs <- c("foo", "bar")
    exp <- paste0("gs://", obs)
    expect_identical(.gcs_pathify(obs), exp)
    
    obs1 <- c("gs://foo", "bar")
    expect_identical(.gcs_pathify(obs1), exp)

    obs2 <- c("gs://foo", "gs://bar")
    expect_identical(.gcs_pathify(obs2), exp)
})

test_that(".install_find_dependencies works", {
    dir.create(lib <- tempfile())

    pkgs <- character()
    expect_identical(.install_find_dependencies(pkgs, lib), pkgs)

    pkgs <- "BiocVersion"
    expect_identical(.install_find_dependencies(pkgs, lib), pkgs)

    pkgs <- "BiocGenerics"
    exp <- c(pkgs, "methods", "utils", "graphics", "stats", "parallel")
    expect_identical(.install_find_dependencies(pkgs, lib), exp)

    ## search all libraries; other dependencies are required / recommended
    ## so already installed
    exp <- pkgs <- "BiocGenerics"
    expect_identical(.install_find_dependencies(pkgs, NULL), pkgs)
})
