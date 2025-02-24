# setwd("~/bioc/AnVIL")
file_loc <- "inst/service/dockstore/openapi.yaml"

download.file(
    url = "https://dockstore.org/api/openapi.yaml",
    destfile = file_loc
)

docklines <- readLines("R/Dockstore.R")

.DOCKSTORE_LINE <- ".DOCKSTORE_API_REFERENCE_VERSION <-"

verline <- grep(
    pattern = .DOCKSTORE_LINE,
    x = docklines,
    fixed = TRUE,
    value = TRUE
)
oldver <- unlist(strsplit(verline, "\""))[[2L]]
newver <- yaml::read_yaml(file_loc)[[c("info", "version")]]

## success -- updated API files and MD5
if (!identical(oldver, newver)) {
    ## update the version in the R file
    lineIdx <- grep(.DOCKSTORE_LINE, docklines, fixed = TRUE)
    docklines[lineIdx] <- paste0(
        .DOCKSTORE_LINE, " \"", newver, "\""
    )
    writeLines(docklines, con = file("R/Dockstore.R"))

    ## update the API file
    oldwd <- setwd("inst/service/dockstore")
    on.exit(setwd(oldwd))
    system2(
        command = "api-spec-converter",
        args = "-f openapi_3 -t swagger_2 openapi.yaml > api.yaml",
        stdout = TRUE
    )

    quit(status = 0)
} else {
    ## failure -- API the same
    quit(status = 1)
}