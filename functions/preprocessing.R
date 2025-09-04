# This script is used to clean and manipulate data for pvsca project

# load library
library(Seurat)
library(tidyverse)
library(janitor)

setwd("/srv/baia/prj/pvsca")
# load gff data
gff_data <- readRDS("ref/PvivaxP01_gff_data.rds")

# load seurat object
so <- readRDS("./pre_process_data/pv_all_studies.rds")

metadata <- colnames(so@meta.data)

# subset reduction columns from seurat object
umap <- colnames(so@reductions$umap_unintegrated@cell.embeddings)
umap_int <- colnames(so@reductions$umap_integrated@cell.embeddings)
pca <- colnames(so@reductions$pca_unintegrated@cell.embeddings)[1:3]
pca_int <- colnames(so@reductions$pca_integrated@cell.embeddings)[1:3]
tsne <- colnames(so@reductions$tsne_integrated@cell.embeddings)

# Extract metadata and reduction data
mr_data <- FetchData(
  so,
  vars = c(
    metadata,
    umap,
    umap_int,
    pca,
    pca_int,
    tsne
  )
) %>%
  clean_names()

# clean mr_data
mr_data <- mr_data %>%
  dplyr::rename(
    umap_i_1 = ytdeh_1,
    umap_i_2 = ytdeh_2,
    umap_i_3 = ytdeh_3,
    umap_u_1 = umap_1,
    umap_u_2 = umap_2,
    umap_u_3 = umap_3,
    pca_i_1 = harmony_1,
    pca_i_2 = harmony_2,
    pca_i_3 = harmony_3,
    pca_u_1 = pc_1,
    pca_u_2 = pc_2,
    pca_u_3 = pc_3
  )


# reduction and metadata
red_df <- mr_data %>%
  select(
    any_of(metadata),
    any_of(contains("umap")),
    any_of(contains("pca")),
    any_of(contains("tsne"))
  )
# sort by row names
red_df <- red_df[order(rownames(red_df)), ]

# normalized data
norm_exp <- GetAssayData(so, slot = "data")
norm_exp <- Matrix::t(norm_exp)
norm_exp <- norm_exp[order(rownames(norm_exp)), ]

# combined normalized data
normalize_df <- cbind(red_df, as.matrix(norm_exp))


# raw data
raw_exp <- GetAssayData(so, layer = "counts")
raw_exp <- Matrix::t(raw_exp)
raw_exp <- raw_exp[order(rownames(raw_exp)), ]

# combined raw data
raw_df <- cbind(red_df, as.matrix(raw_exp))

# scale data
scale_exp <- GetAssayData(so, layer = "scale")
scale_exp <- Matrix::t(scale_exp)
scale_exp <- scale_exp[order(rownames(scale_exp)), ]

# combined raw data
scale_df <- cbind(red_df, as.matrix(scale_exp))

# save normalized, raw and scaled data
saveRDS(normalize_df, "data/normalize_df.rds")
saveRDS(raw_df, "data/raw_df.rds")
saveRDS(scale_df, "data/scale_df.rds")


# Extract PCA standard deviation
pca_std_df <- so@reductions$pca_unintegrated@stdev

pca_std_df <- data.frame(
  pc = 1:length(pca_std_df),
  std = pca_std_df
)

# variable genes expression
so <- FindVariableFeatures(
  so,
  assay = "RNA",
  selection.method = "vst",
  nfeatures = 2000
)
vf_data <- HVFInfo(object = so, assay = "RNA", method = "vst", status = TRUE)

vf_data <- vf_data %>%
  rownames_to_column("id") %>%
  left_join(gff_data, by = "id") %>%
  select(id, name, mean, variable, rank, variance.standardized) %>%
  mutate(name = coalesce(name, id))

# top expression genes
top_genes <- rowMeans(GetAssayData(so, assay = "RNA", layer = "data"))
top_genes <- names(sort(top_genes, decreasing = TRUE)[1:10])

# top expression genes with cluster information
top_genes_exp <- FetchData(so, vars = c(top_genes, "ident")) %>%
  rownames_to_column("id") %>%
  pivot_longer(
    any_of(top_genes),
    names_to = "gene",
    values_to = "expression"
  ) %>%
  left_join(gff_data, by = c("gene" = "id")) %>%
  select(id, gene, name, expression, ident) %>%
  mutate(name = coalesce(name, gene))

save_data <- list(
  mr_data = mr_data,
  pca_std_df = pca_std_df,
  vf_data = vf_data,
  top_genes_exp = top_genes_exp
)

# save list of data
saveRDS(save_data, "data/cleaned_dataset.rds")

# clear objects
rm(list = ls())
