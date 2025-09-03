library(DropletUtils)
library(tidyverse)
library(rtracklayer)
library(Seurat)
library(scater)
library(SingleCellExperiment)

setwd("/srv/baia/prj/pvsca/pre_process_data")

rds_files <- list.files(pattern = "^hazzard2024_pv.*\\.rds$")

hazzard2022_list <- lapply(rds_files, readRDS)

# Merge all
pv.combined.all <- Reduce(function(x, y) merge(x, y = y), hazzard2022_list)

# Join layers if needed (multi-layer data)
pv.combined.all <- JoinLayers(pv.combined.all)

# Standard preprocessing
# pv.combined.all <- NormalizeData(pv.combined.all)
# pv.combined.all <- FindVariableFeatures(
#   pv.combined.all,
#   selection.method = "vst",
#   nfeatures = nrow(pv.combined.all) * 0.3,
#   verbose = FALSE
# )
# pv.combined.all <- ScaleData(pv.combined.all)
# pv.combined.all <- RunPCA(pv.combined.all)

# Save the final object
saveRDS(pv.combined.all, file = "hazzard2024.rds")

# Cleanup
rm(list = ls())
gc()
