# this script is use to pre processing data

# load library
library(Seurat)
library(tidyverse)
library(janitor)

# load gff data
gff_data <- read_tsv("PvivaxP01.gff_data.tsv") %>% 
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
  "Sample", "seurat_clusters",
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

# save merge data
saveRDS(merg_data, "data/pvsca_data.rds")

# Identify the top 2000 variable genes
vf <- FindVariableFeatures(so, selection.method = "vst", nfeatures = 2000)

ndata <- GetAssayData(so, assay = "RNA", slot = "data")
mean_expression <- rowMeans(ndata)
variance_expression <- apply(ndata, 1, var)
std_variance <-
  (variance_expression - mean(variance_expression)) / sd(variance_expression)

vf_data <- data.frame(
  gene = rownames(ndata),
  mean_expr = mean_expression,
  variance_expr = std_variance
)

vf_top10 <- head(VariableFeatures(so), 10)

variable_genes <- vf_data[vf_data$gene %in% VariableFeatures(so), ]
top10_genes <- vf_data[vf_data$gene %in% vf_top10, ]
total_genes <- vf_data

plot_ly() %>%
  add_trace(
    data = total_genes,
    x = ~mean_expr,
    y = ~variance_expr,
    text = ~gene,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 6, color = 'lightgray'),
    hoverinfo = 'text',
    name = paste('Non-variable: (', nrow(total_genes), ')', sep = '')
  ) %>%
  add_trace(
    data = variable_genes,
    x = ~mean_expr,
    y = ~variance_expr,
    text = ~gene,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 8, color = 'blue'),
    hoverinfo = 'text',
    name = paste('Variable: (', nrow(variable_genes), ')', sep = '')
  ) %>%
  add_trace(
    data = top10_genes,
    x = ~mean_expr,
    y = ~variance_expr,
    text = ~gene,
    type = 'scatter',
    mode = 'markers+text',
    marker = list(size = 10, color = 'red'),
    textposition = 'top',
    hoverinfo = 'text',
    name = paste('Top 10 Variable Genes (', nrow(top10_genes), ')', sep = '')
  ) %>%
  layout(
    title = "Variable Features Plot",
    xaxis = list(title = "Average Expression"),
    yaxis = list(title = "Standardized Varaince"),
    showlegend = TRUE
  )


