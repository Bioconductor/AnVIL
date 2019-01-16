devtools::load_all()
library(dplyr)

response <- leonardo$createClusterV2 (
    googleProject = "anvil-leo-dev",
    clusterName = "mtmorganbioc",
    rstudioDockerImage = "us.gcr.io/anvil-leo-dev/anvil_bioc_docker:latest"
)

leonardo$listClusters() %>% flatten() %>%
    select(creator, status, clusterUrl) %>%
    filter(grepl("mtm", creator))


url <- content(response)$clusterUrl
url <- sub("130.211.229.19", "leonardo.dev.anvilproject.org", url)
url <- paste0(url, "/rstudio")

stop_response <- leonardo$stopCluster(
             googleProject = "anvil-leo-dev",
             clusterName = "mtmorganbioc"
         )
