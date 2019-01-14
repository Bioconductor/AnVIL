This archive currently contains an interface to AnVIL web
services. The interface uses [rapiclient][7] (rather than older
[CRAN][6] version), and is easily extended. Current implementations
are for Leonardo and Terra; there are Gen3 stubs but these are not
complete because I do not know end points.

The package provides singleton endpoints with tab completion on operations

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

[3b]: http://editor.swagger.io/#/
[6]: https://cran.r-project.org/package=rapiclient
[7]: https://github.com/bergant/rapiclient
