# this script is use to pre processing data

# load library
library(Seurat)
library(tidyverse)
library(janitor)

# load gff data
gff_data <- read_tsv("ref/PvivaxP01_gff_data.tsv") %>%
  clean_names()

# load seurat object
so <- readRDS("data/Pv.all.12.Jan2025.rds")

# change features from - to _ to match with gff gene_id
features <- gsub("-", "_", rownames(so[["RNA"]]@features))
rownames(so[["RNA"]]@features) <- features
saveRDS(so, "data/Pv.all.12.Jan2025.modified.rds")

# subset columns from metadata
metadata <- c(
  "species", "day", "condition", "Stage", "Replicate", "is_outlier",
  "location", "Vivax_strain", "nCount_RNA", "nFeature_RNA",
  "Sample", "seurat_clusters", grep("CCA_snn", colnames(so@meta.data),
    ignore.case = TRUE, value = TRUE
  )
)

# subset reduction from seurat object
umap <- colnames(so@reductions$umap@cell.embeddings)
umap_int <- colnames(so@reductions$umap.unintegrated@cell.embeddings)
pca <- colnames(so@reductions$pca@cell.embeddings)[1:3]
pca_int <- colnames(so@reductions$integrated.rpca@cell.embeddings)[1:3]
tsne <- colnames(so@reductions$tsne@cell.embeddings)

# top highly expressed genes
mean_exp <- rowMeans(GetAssayData(so, assay = "RNA", layer = "data"))

top_features <- names(sort(mean_exp, decreasing = TRUE))

top_features_name <- gff_data$name[match(top_features, gff_data$id)]
top_features_name <- ifelse(is.na(top_features_name), top_features,
  top_features_name
)

mr_data <- FetchData(so, vars = c(
  metadata, "ident", umap, umap_int, pca,
  pca_int, tsne, top_features
)) %>%
  clean_names()

colnames(mr_data)[tail(seq_along(mr_data), length(mean_exp))] <- top_features

mr_data <- mr_data %>%
  rownames_to_column("id") %>%
  mutate(
    species = case_when(
      species == "vivax" ~ "Vivax",
      TRUE ~ species
    ),
    stage = case_when(
      stage == "BloodStage" ~ "human_blood",
      stage == "Liver Stage" ~ "human_liver",
      stage == "Sporozoite" ~ "mosquito_saliva",
      TRUE ~ stage
    )
  )

# Extract all features
# GetAssayData(so, assay = "RNA", layer = "data") %>%
#   as_tibble() %>%
#   rownames_to_column("gene") %>%
#   left_join(gff_data, by = c("gene" = "id")) %>%
#   mutate(name = coalesce(name, gene)) %>%
#   select(gene, name, description, everything(), -ebi_biotype) %>%
#   saveRDS("data/all_features.rds")

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

# differential expression genes
# de_genes <- FindAllMarkers(so)
#
# de_genes <- left_join(de_genes, gff_data, by = c("gene" = "id")) %>%
#   select(gene, name, everything())
# saveRDS(de_genes, "data/de_genes.rds")

# clear objects
rm(list = ls())
