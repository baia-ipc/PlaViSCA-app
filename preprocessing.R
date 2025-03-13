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

# variable genes expression
ndata <- GetAssayData(so, assay = "RNA", slot = "data")
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
  
# saveRDS(ndata, "data/variable_genes.rds")
saveRDS(ndata, "data/variable_genes.rds")


# 
# plot_ly() %>%
#   add_trace(
#     data = ndata,
#     x = ~mean_expr,
#     y = ~std_variance,
#     text = ~name,
#     type = 'scatter',
#     mode = 'markers',
#     marker = list(size = 6, color = 'lightgray'),
#     hoverinfo = 'text',
#     name = paste('Non-variable: (', nrow(ndata), ')', sep = '')
#   ) %>%
#   add_trace(
#     data = ndata[ndata$variable_genes, ],
#     x = ~mean_expr,
#     y = ~std_variance,
#     text = ~name,
#     type = 'scatter',
#     mode = 'markers',
#     marker = list(size = 8, color = 'blue'),
#     hoverinfo = 'text',
#     name = paste('Variable: (', nrow(ndata[ndata$variable_genes, ]), ')',
#                  sep = '')
#   ) %>%
#   add_trace(
#     data = ndata[ndata$top, ],
#     x = ~mean_expr,
#     y = ~std_variance,
#     text = ~name,
#     type = 'scatter',
#     mode = 'markers+text',
#     marker = list(size = 10, color = 'red'),
#     textposition = 'top',
#     hoverinfo = 'text',
#     name = paste('Top Variable Genes (', nrow(ndata[ndata$top, ]), ')', sep = '')
#   ) %>%
#   layout(
#     title = "Variable Features Plot",
#     xaxis = list(title = "Average Expression"),
#     yaxis = list(title = "Standardized Varaince"),
#     showlegend = TRUE
#   )
# 
# 
