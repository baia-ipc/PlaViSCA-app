# this script is use to pre processing data

# load library
library(Seurat)
library(tidyverse)
library(janitor)

# load seurat object
so <- readRDS("data/Pv.all.12.Jan2025.rds")

# subset columns from metadata
metadata <- so@meta.data
metadata_subset <- metadata[, c(
  "species", "day", "condition", "Stage","Replicate",
  "is_outlier", "location", "Vivax_strain", "nCount_RNA", "nFeature_RNA",
  grep("CCA_snn", colnames(metadata),
    value = TRUE
  )
)]

metadata_subset <- metadata_subset %>%
  rownames_to_column("id") %>%
  mutate(species = case_when(
    species == "vivax" ~ "Vivax",
    TRUE ~ species
  ),
  Stage = case_when(
    Stage == "BloodStage" ~ "human_blood",
    Stage == "Liver Stage" ~ "human_liver",
    Stage == "Sporozoite" ~ "mosquito_saliva",
    TRUE ~ Stage
  )
  )

# subset reduction from seurat object
umap_data <- as.data.frame(so@reductions$umap@cell.embeddings) %>%
  rownames_to_column("id")
pca_data <- as.data.frame(so@reductions$pca@cell.embeddings) %>%
  rownames_to_column("id")
tsne_data <- as.data.frame(so@reductions$tsne@cell.embeddings) %>%
  rownames_to_column("id")

# merge metadata and umap data
merg_data <- reduce(list(metadata_subset, umap_data, pca_data, tsne_data),
                         full_join, by = "id") %>%
  janitor::clean_names()

# save output
saveRDS(merg_data, "data/pvsca_data.rds")
