This archive contains an interface to AnVIL web services. The
interface uses [Bioconductor/AnVIL_rapiclient][7] (rather than the
older [CRAN][6] version), and is easily extended. Current
implementations are for Leonardo, Terra, and Dockstore; there are Gen3
stubs but these are not complete because I do not know end points.

Install the dependency and this package with

    pkgs <- ("Bioconductor/AnVIL_rapiclient", "Bioconductor/AnVIL")
    BiocManager::install(pkgs)

The package provides singleton endpoints with tab completion on
operations

    terra = Terra()
    terra$getUserStatus() %>% AnVIL::str()

    leonardo = Leonardo()
    leonardo
    leonardo$listClusters() %>% AnVIL::flatten()
    operations(leonardo)
    schemas(leonardo)

The return values all require further processing; there are some
helper functions `str()` and `flatten()`, as well as
`httr::content()`.

Some services require client keys, in
`inst/service/<name>/auth.json`.

- For dockstore, visit [dockstore.org/accounts][8] and put your token in
  `inst/service/dockstore/auth.json` file like

    ```
    {
        "token" : "fff"
    }
    ```

[6]: https://cran.r-project.org/package=rapiclient
[7]: https://github.com/Bioconductor/AnVIL_rapiclient
[8]: https://dockstore.org/accounts
