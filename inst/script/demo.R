devtools::load_all()
library(dplyr)

## Test 1
## Test docker hub hosted container
response <- leonardo$createClusterV2 (
    googleProject = "anvil-leo-dev",
    clusterName = "nitesh7",
    rstudioDockerImage = "bioconductor/anvil-rstudio-bioc:3.9"
)

response

## Filter to see the status

leonardo$listClusters() %>% flatten() %>%
    select(creator, status, clusterUrl) %>%
    filter(grepl("nitesh", creator), grepl("nitesh7", clusterUrl))


url <- content(response)$clusterUrl
url <- sub("130.211.229.19", "leonardo.dev.anvilproject.org", url)
url <- paste0(url, "/rstudio")

url

stop_response <- leonardo$stopCluster(
             googleProject = "anvil-leo-dev",
             clusterName = "nitesh6"
         ) %>% content()


## Fails as well
stop_response


## Test 2
## Test images hosted on GCR

response <- leonardo$createClusterV2 (
    googleProject = "anvil-leo-dev",
    clusterName = "nitesh7",
    rstudioDockerImage = "us.gcr.io/anvil-leo-dev/anvil_bioc_docker:latest"
)
