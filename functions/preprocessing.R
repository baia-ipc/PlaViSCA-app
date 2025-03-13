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
metadata <- so@meta.data
metadata_subset <- metadata[, c(
  "species", "day", "condition", "Stage","Replicate",
  "is_outlier", "location", "Vivax_strain", "nCount_RNA", "nFeature_RNA",
  "Sample", "seurat_clusters"
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
meta_reduc_data <- reduce(list(metadata_subset, umap_data, pca_data, tsne_data),
                         full_join, by = "id") %>%
  janitor::clean_names()

# variable genes expression
ndata <- GetAssayData(so, assay = "RNA", layer = "data")
mean_expression <- rowMeans(ndata)
variance_expression <- apply(ndata, 1, var)
std_variance <-
  (variance_expression - mean(variance_expression)) / sd(variance_expression)

ndata <- data.frame(
  gene = rownames(ndata),
  mean_expr = mean_expression,
  std_variance = std_variance
)

variable_genes <- VariableFeatures(so)
vf_top <- head(variable_genes, 15)

ndata <- ndata %>% 
  mutate(variable_genes = gene %in% variable_genes,
         top = gene %in% vf_top)

ndata <- left_join(ndata, gff_data, by = c("gene" = "id")) %>%
  select(gene, name, mean_expr, std_variance, variable_genes, top) %>% 
  mutate(name = coalesce(name, gene))
  


# top expression genes
top_genes <- rowMeans(GetAssayData(so, assay = "RNA", layer = "data"))
top_genes <- names(sort(top_genes, decreasing = TRUE)[1:10])

# top expression genes with cluster information
top_genes_exp <- FetchData(so, vars = c(top_genes, "seurat_clusters")) %>% 
  rownames_to_column("id") %>% 
  pivot_longer(any_of(top_genes), names_to = "gene", values_to = "expression") %>% 
  left_join(gff_data, by = c("gene" = "id")) %>%
  select(gene, name, expression, seurat_clusters) %>% 
  mutate(name = coalesce(name, gene))

save_data <- list(
  mr_data = meta_reduc_data,
  normalized_genes_var = ndata,
  top_genes_exp = top_genes_exp
)

# save list of data
saveRDS(save_data, "data/cleaned_dataset.rds")

# clear objects
rm(list = ls())

