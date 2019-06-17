context("localize")

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
