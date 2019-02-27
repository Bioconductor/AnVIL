This archive currently contains an interface to AnVIL web
services. The interface uses [bergant/rapiclient][7] (rather than the
older [CRAN][6] version), and is easily extended. Current
implementations are for Leonardo, Terra, and Dockstore; there are Gen3
stubs but these are not complete because I do not know end points.

The package provides singleton endpoints with tab completion on
operations

    leonardo
    terra$getServiceStatus()
    leonardo$listClusters() %>% content(as = "text") %>%
        fromJSON(flatten = TRUE) %>% as_tibble()
    operations(leonardo)
    schemas(leonardo)

The return values all require further processing (`httr::content() %>%
...`).

The api definititions `inst/service/<name>/api.json` needs to be json
rather than YAML, see [swagger-codegen online][3b]).

Some services require client keys, in
`inst/service/<name>/auth.json`.

- For Leonardo and Terra, visit [here][1] and download (click on the
  downward-facing arrow to the right) the "Bioconductor-AnVIL"
  credentials to a file `inst/service/{leonardo,terra}/auth.json`.

- For dockstore, visit [docstore.org/accounts][] and put your token in
  `inst/service/dockstore/auth.json` file like

    ```
    {
        "token" : "fff"
    }
    ```

[1]: https://console.cloud.google.com/apis/credentials?authuser=1&project=anvil-leo-dev
[3b]: http://editor.swagger.io/#/
[6]: https://cran.r-project.org/package=rapiclient
[7]: https://github.com/bergant/rapiclient
[docstore.org/accounts]: https://dockstore.org/accounts
