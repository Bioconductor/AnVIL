This archive currently contains an interface to AnVIL web
services. The interface uses [Bioconductor/AnVIL_rapiclient][7] (rather 
than the older [CRAN][6] version), and is easily extended. Current
implementations are for Leonardo, Terra, and Dockstore; there are Gen3
stubs but these are not complete because I do not know end points.

Install the dependency with

    BiocManager::install("Bioconductor/AnVIL_rapiclient")

Install the AnVIL package from a clone of this repository, adding
`inst/service/<name>/auth.json` as described below.

The package provides singleton endpoints with tab completion on
operations

    leonardo
    terra$getUserStatus()
    leonardo$listClusters() %>% content(as = "text") %>%
        fromJSON(flatten = TRUE) %>% as_tibble()
    operations(leonardo)
    schemas(leonardo)

The return values all require further processing; there are some
helper functions `str()` and `flatten()`, as well as
`httr::content()`.

Some services require client keys, in
`inst/service/<name>/auth.json`.

- For dockstore, visit [dockstore.org/accounts][] and put your token in
  `inst/service/dockstore/auth.json` file like

    ```
    {
        "token" : "fff"
    }
    ```

[6]: https://cran.r-project.org/package=rapiclient
[7]: https://github.com/Bioconductor/AnVIL_rapiclient
[docstore.org/accounts]: https://dockstore.org/accounts
