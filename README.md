This archive currently contains an interface to the Leonardo web
service. The interface was created by scraping the [swagger][1] API
'by hand'. A better alternative would build the API programmatically
from the underlying [YAML description][2]

A couple of other alternatives have been
investigated. [swagger-codegen][3a] (or its [online equivalent][3b])
can parse the yaml to other languages, including R. However, the R
implementation is not effective -- there are minor bugs in the
translation (e.g., the variable `_labels` cannot be used in R), but
also parsing the results model fails:

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

The package [rapiclient][6] ([github][7]) generates clients from the
swagger json.  The Leonardo api-docs.yaml file can be transformed to
JSON using swagger-codegen. So

    > library(rapiclient)
    > library(httr)
    > library(tidyverse)
    > api <- get_api("anvil-leonardo-api.json")
    > api$host <- "leonardo.dev.anvilproject.org"
    > api$schemes  <- "https"
    > operations <- get_operations(api)
    > names(operations)

     [1] "ping"                  "getSystemStatus"       "listClusters"
     [4] "listClustersByProject" "createClusterV2"       "getCluster"
     [7] "createCluster"         "deleteCluster"         "stopCluster"
    [10] "startCluster"          "proxyCluster"          "proxyLocalize"
    [13] "setCookie"             "invalidateToken"
    > operations$listClusters
    listClusters
    List all active clusters
    Description:
       List all active clusters, optionally filtering on a set of labels

    Parameters:
      my-labels (string)
        Optional label key-value pairs to filter results by. Example: Querying by key1=val1,key2=val2
    returns all clusters that contain the key1/val1 and key2/val2 labels (possibly among other labels).
    Note: this string format is a workaround because Swagger doesn't support free-form
    query string parameters. The recommended way to use this endpoint is to specify the
    labels as top-level query string parameters. For instance: GET /api/clusters?key1=val1&key2=val2.

      includeDeleted (boolean)
        Optional filter that includes any clusters with a Deleted status.
    > response <- with_config(c(
    +     anvil_options("leonardo_config"),
    +     config(token = AnVIL::authenticate())
    + ), {
    +     operations$listClusters()
    + })
    response
    >
    Response [https://leonardo.dev.anvilproject.org/api/clusters]
      Date: 2019-01-06 10:30
      Status: 200
      Content-Type: application/json
      Size: 13.5 kB

    > value <- content(response, as="text")

    No encoding supplied: defaulting to UTF-8.
    > fromJSON(value, flatten=TRUE) %>% as_tibble()

    # A tibble: 17 x 24
       autopauseThresh… stagingBucket clusterImages scopes instances errors creator
     *            <int> <chr>         <list>        <list> <list>    <list> <chr>
     1               30 leostaging-v… <list [0]>    <list… <list [0… <list… stvjc@…
     2               30 leostaging-n… <list [0]>    <list… <list [0… <list… nitesh…
     3               30 leostaging-r… <list [0]>    <list… <list [0… <list… reshg@…
     4               30 leostaging-n… <list [0]>    <list… <list [0… <list… nitesh…
     5               30 NA            <list [0]>    <list… <list [0… <list… reshg@…
     6               30 leostaging-b… <list [0]>    <list… <list [0… <list… rebjh@…
     7               30 leostaging-r… <list [0]>    <list… <list [0… <list… rtitle…
     8               30 leostaging-e… <list [0]>    <list… <list [0… <list… afgane…
     9               30 NA            <list [0]>    <list… <list [0… <list… nitesh…
    10               30 leostaging-l… <list [0]>    <list… <list [0… <list… stvjc@…
    11               30 NA            <list [0]>    <list… <list [0… <list… reshg@…
    12               30 leostaging-j… <list [0]>    <list… <list [0… <list… rebjh@…
    13               30 leostaging-n… <list [0]>    <list… <list [0… <list… nitesh…
    14               30 NA            <list [0]>    <list… <list [0… <list… stvjc@…
    15               30 leostaging-a… <list [0]>    <list… <list [0… <list… nitesh…
    16               30 leostaging-b… <list [0]>    <list… <list [0… <list… nitesh…
    17               30 leostaging-v… <list [0]>    <list… <list [0… <list… stvjc@…
    # ... with 17 more variables: googleProject <chr>, id <int>,
    #   dateAccessed <chr>, stopAfterCreation <lgl>, status <chr>,
    #   clusterUrl <chr>, clusterName <chr>, hostIp <chr>, operationName <chr>,
    #   googleId <chr>, createdDate <chr>, machineConfig.numberOfWorkers <int>,
    #   machineConfig.masterMachineType <chr>, machineConfig.masterDiskSize <int>,
    #   labels.creator <chr>, labels.clusterName <chr>, labels.googleProject <chr>
    >

The implementation of _rapiclient_ could be modified (see [Martin's
fork][8]) to allow specification of config arguments on api creation.

    api <- get_api(
        "anvil-leonardo-api.json",
        config = c(
            AnVIL::anvil_options("leonardo_config"),
            config(token = AnVIL::authenticate())
        )
    )
    api$host <- "leonardo.dev.anvilproject.org"
    api$schemes  <- "https"

    operations <- get_operations(api)
    operations$listClusters()

[1]: https://leonardo.dev.anvilproject.org/
[2]: https://leonardo.dev.anvilproject.org/api-docs.yaml
[3a]: https://swagger.io/tools/swagger-codegen/
[3b]: http://editor.swagger.io/#/
[4]: https://github.com/swagger-api/swagger-codegen
[5]: https://github.com/swagger-api/swagger-codegen/blob/e15bbc961e028162ad288bec66b2b08d24ef5fd7/modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/RClientCodegen.java#L92
[6]: https://cran.r-project.org/package=rapiclient
[7]: https://github.com/bergant/rapiclient
[8]: https://github.com/mtmorgan/rapiclient/tree/httr-config-support
