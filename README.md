This archive currently contains an interface to the Leonardo web
service. The interface was created by scraping the [swagger][1] API
'by hand'. A better alternative would build the API programmatically
from the underlying [YAML description][2]

A version using [rapiclient][7] is available on the [rapiclient branch][9].

Another alternatives has been investigated. [swagger-codegen][3a] (or
its [online equivalent][3b]) can parse the yaml to other languages,
including R. However, the R implementation is not effective -- there
are minor bugs in the translation (e.g., the variable `_labels` cannot
be used in R), but also parsing the results model fails:

    > apiClient <- ApiClient$new(basePath = "https://leonardo.dev.anvilproject.org")
    > clusterApi <- ClusterApi$new(apiClient = apiClient)
    > auth <- config(token = authenticate())   # from AnVIL package
    > response <-
    +     clusterApi$list_clusters(NULL, NULL, auth, anvil_options("leonardo_config"))
    >
    Error in returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8")) (from Cluster.r#231) :
      object 'TODO_OBJECT_MAPPING' not found
    >

This tool is on [github][4], and it would not be inconievable to
create a more functional fork, e.g., returning simple json rather than
trying for a complicated object mapping at the [location][5] where the
above error occurs.

[1]: https://leonardo.dev.anvilproject.org/
[2]: https://leonardo.dev.anvilproject.org/api-docs.yaml
[3a]: https://swagger.io/tools/swagger-codegen/
[3b]: http://editor.swagger.io/#/
[4]: https://github.com/swagger-api/swagger-codegen
[5]: https://github.com/swagger-api/swagger-codegen/blob/e15bbc961e028162ad288bec66b2b08d24ef5fd7/modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/RClientCodegen.java#L92
[7]: https://github.com/bergant/rapiclient
[9]: https://github.com/Bioconductor/AnVIL/tree/rapiclient
