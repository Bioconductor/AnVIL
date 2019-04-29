devtools::load_all()
library(dplyr)

namespace="fccredits-indium-azure-9792"
workspace="TCGA_LUSC_hg38_OpenAccess_GDCDR-12-0_DATA_copy"

terra$getEntityTypes(namespace, workspace) %>% str()
