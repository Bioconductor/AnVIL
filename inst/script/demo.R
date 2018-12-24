devtools::load_all()
library(tidyverse)

tbl1 <- api_clusters()
tbl1

tbl1 %>% head(1) %>% t()

tbl1 %>% select(starts_with("label"))

tbl2 <- api_clusters(creator = "nitesh.turaga@gmail.com")

tbl2

clusterName <- tbl2 %>%
    select("labels.clusterName") %>%
    head(1) %>%
    as.character()

clusterName

api_cluster(clusterName = clusterName) %>%
    str()
