# this script is use to pre processing data

# load library
library(Seurat)
library(tidyverse)

# load seurat object
so <- readRDS("Pv.all.12.Jan2025.rds")

# subset columns from metadata
metadata <- so@meta.data
metadata_subset <- metadata[, c(
  "species", "day", "condition", "Stage",
  "is_outlier",
  grep("CCA_snn", colnames(metadata),
    value = TRUE
  ), "location", "Vivax_strain"
)]

# subset reduction from seurat object
umap_data <- so@reductions$umap@cell.embeddings
pca_data <- so@reductions$pca@cell.embeddings
tsne_data <- so@reductions$tsne@cell.embeddings
lsdf <- list(umap_data, pca_data, tsne_data)

# merge metadata and umap data
output_data <- reduce(lsdf, merge, by = "row.names", all.x = TRUE)
output_data <- output_data[, -1] %>% view()

# save output
saveRDS(output_data, "output_data.rds")
