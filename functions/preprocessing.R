# This script is used to pre processing data for pvsca project

# load library
library(Seurat)
library(tidyverse)
library(janitor)

# load gff data
gff_data <- read_tsv("ref/PvivaxP01_gff_data.tsv") %>%
  clean_names()

# load parasite sex data
type_df <- read.csv("data/pbio_3000711_s034.csv") %>%
  clean_names() %>%
  select(sample, type) %>%
  mutate(sample = gsub(".*_", "", sample))

# load seurat object
so <- readRDS("data/Pv.all.12.Jan2025.rds")

# change features from - to _ to match with gff gene_id
features <- gsub("-", "_", rownames(so[["RNA"]]@features))
rownames(so[["RNA"]]@features) <- features

# add column type as parasite sex to metadata
col <- colnames(so)
cell_id_af_ <- gsub(".*_", "", col)
cell_id_af_[nchar(cell_id_af_) < 10] <- NA

cell_id_bf_ <- gsub("_.*", "", col)
cell_id_bf_[nchar(cell_id_bf_) < 10] <- NA

cell_id_clean <- coalesce(cell_id_af_, cell_id_bf_)
so@meta.data$cleaned_barcode <- cell_id_clean

so@meta.data$type <- type_df$type[match(
  so@meta.data$cleaned_barcode,
  type_df$sample
)]
so@meta.data$cleaned_barcode <- NULL

# saveRDS(so, "data/Pv.all.12.Jan2025.modified.rds")

# subset columns from metadata
metadata <- c(
  "species", "day", "condition", "Stage", "Replicate", "type",
  "location", "Vivax_strain", "nCount_RNA", "nFeature_RNA",
  "Sample", "seurat_clusters", grep("CCA_snn", colnames(so@meta.data),
    ignore.case = TRUE, value = TRUE
  )
)

# subset reduction columns from seurat object
umap <- colnames(so@reductions$umap@cell.embeddings)
umap_int <- colnames(so@reductions$umap.unintegrated@cell.embeddings)
pca <- colnames(so@reductions$pca@cell.embeddings)[1:3]
pca_int <- colnames(so@reductions$integrated.rpca@cell.embeddings)[1:3]
tsne <- colnames(so@reductions$tsne@cell.embeddings)

# Extract metadata and reduction data
mr_data <- FetchData(so, vars = c(
  metadata, "ident", umap, umap_int, pca,
  pca_int, tsne
)) %>%
  clean_names() %>%
  rename(
    umap_i_1 = umap_1,
    umap_i_2 = umap_2,
    umap_i_3 = umap_3,
    umap_u_1 = umapunintegrated_1,
    umap_u_2 = umapunintegrated_2,
    umap_u_3 = umapunintegrated_3,
    pca_i_1 = integratedrpca_1,
    pca_i_2 = integratedrpca_2,
    pca_i_3 = integratedrpca_3,
    pca_u_1 = pc_1,
    pca_u_2 = pc_2,
    pca_u_3 = pc_3
  )

# clean mr_data
mr_data <- mr_data %>%
  rownames_to_column("id") %>%
  mutate(
    species = case_when(
      species == "vivax" ~ "Vivax",
      TRUE ~ species
    ),
    stage = case_when(
      stage == "BloodStage" ~ "blood",
      stage == "Liver Stage" ~ "liver",
      stage == "Sporozoite" ~ "sporozoite",
      TRUE ~ stage
    )
  )

# Extract reduction data from mr_data
red_df <- mr_data %>%
  select(
    id, stage, species, condition, sample, type, ident, n_feature_rna,
    any_of(make_clean_names(umap)),
    any_of(make_clean_names(umap_int)),
    any_of(make_clean_names(pca)),
    any_of(make_clean_names(pca_int)),
    any_of(make_clean_names(tsne))
  )

# Extract genes for normalize data in average of expression
norm_exp <- GetAssayData(so, layer = "data")
norm_exp <- as.data.frame(t(as.matrix(norm_exp))) %>%
  tibble::rownames_to_column("id")
normalize_df <- left_join(red_df, norm_exp, by = "id")

# Extract genes for raw count data
raw_exp <- GetAssayData(so, layer = "counts")
raw_exp <- as.data.frame(t(as.matrix(raw_exp))) %>%
  tibble::rownames_to_column("id")
raw_df <- left_join(red_df, raw_exp, by = "id")

# Extract genes for scale data
scale_exp <- GetAssayData(so, layer = "scale")
scale_exp <- as.data.frame(t(as.matrix(scale_exp))) %>%
  tibble::rownames_to_column("id")
scale_df <- left_join(red_df, scale_exp, by = "id")

# save normalized, raw and scaled data
saveRDS(normalize_df, "data/normalize_df.rds")
saveRDS(raw_df, "data/raw_df.rds")
saveRDS(scale_df, "data/scale_df.rds")

# Extract PCA standard deviation
pca_std_df <- so@reductions$pca@stdev
pca_std_df <- data.frame(
  pc = 1:length(pca_std_df),
  std = pca_std_df
)

# variable genes expression
so <- FindVariableFeatures(so,
  assay = "RNA", selection.method = "vst",
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
  pivot_longer(any_of(top_genes),
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
