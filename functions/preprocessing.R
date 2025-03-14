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

#change features from - to _ to match with gff gene_id
features <- gsub("-", "_", rownames(so[["RNA"]]@features))
rownames(so[["RNA"]]@features) <- features
saveRDS(so, "data/Pv.all.12.Jan2025.modified.rds")

# subset columns from metadata
metadata <- c("species", "day", "condition", "Stage", "Replicate", "is_outlier",
              "location", "Vivax_strain", "nCount_RNA", "nFeature_RNA",
              "Sample", "seurat_clusters")

# subset reduction from seurat object
umap <- colnames(so@reductions$umap@cell.embeddings)
umap_int <- colnames(so@reductions$umap.unintegrated@cell.embeddings)
pca <- colnames(so@reductions$pca@cell.embeddings)
pca_int <- colnames(so@reductions$integrated.rpca@cell.embeddings)
tsne <- colnames(so@reductions$tsne@cell.embeddings)

# top 10 highly expressed genes
mean_exp <- rowMeans(GetAssayData(so, assay = "RNA", layer = "data"))
top_features <- names(sort(mean_exp, decreasing = TRUE)[1:10])

top_features_name <- gff_data$name[match(top_features, gff_data$id)]
top_features_name <- ifelse(is.na(top_features_name), top_features,
                            top_features_name)

mr_data <- FetchData(so, vars = c(metadata, "ident", umap, umap_int, pca,
                                  pca_int, tsne, top_features)) %>% 
  clean_names() 

colnames(mr_data)[tail(seq_along(mr_data), 10)] <- top_features_name

mr_data <- mr_data %>% 
  rownames_to_column("id") %>%
  mutate(species = case_when(
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

# variable genes expression
so <- FindVariableFeatures(so, assay = "RNA", selection.method = "vst",
                           nfeatures = 2000)
vf_data <- HVFInfo(object = so, assay = "RNA", method = "vst", status = TRUE)


# top expression genes
top_genes <- rowMeans(GetAssayData(so, assay = "RNA", layer = "data"))
top_genes <- names(sort(top_genes, decreasing = TRUE)[1:10])

# top expression genes with cluster information
top_genes_exp <- FetchData(so, vars = c(top_genes, "ident")) %>% 
  rownames_to_column("id") %>% 
  pivot_longer(any_of(top_genes), names_to = "gene", values_to = "expression") %>% 
  left_join(gff_data, by = c("gene" = "id")) %>%
  select(id, gene, name, expression, ident) %>% 
  mutate(name = coalesce(name, gene))

save_data <- list(
  mr_data = mr_data,
  vf_data = vf_data,
  top_genes_exp = top_genes_exp
)



# dims <- c("umapunintegrated_1", "umapunintegrated_2", "umapunintegrated_3")
# cells <- colnames(so)
# features <- rownames(so)
# 
# df <- FetchData(object = so, 
#                 vars = c(umap, umap_int, pca, pca_int, tsne, "ident", top_features), 
#                 cells = cells, 
#                 layer = "data")


# df <- df %>% 
#   pivot_longer(cols = contains("PVP01"), names_to = "gene",
#                values_to = "expression") %>% 
#   arrange(desc(expression))
# 
# head(df)
# 
# mr_data <- mr_data %>% 
#   pivot_longer(cols = contains("PVP01"),
#                names_to = "gene",
#                values_to = "expression") %>% 
#   arrange(desc(expression))
# 
# plot_ly(
#   data = mr_data,
#   x = ~umapunintegrated_1,
#   y = ~umapunintegrated_2,
#   z = ~umapunintegrated_3,
#   color = ~pvp01_0515400,
#   text = ~paste("Cell:", rownames(df), "<br>Cluster:", ident),
#   type = 'scatter',
#   mode = 'markers',
#   marker = list(size = 5, opacity = 0.7)
# ) %>% layout(
#   title = "UMAP colored by PVP01_1132700 expression",
#   xaxis = list(title = "UMAP 1"),
#   yaxis = list(title = "UMAP 2"),
#   zaxis = list(title = "UMAP 3")
# )



# dims <- c("umapunintegrated_1", "umapunintegrated_2", "umapunintegrated_3")
# cells <- colnames(so) 
# features <- c("PVP01_1132700", "PVP01_1440700")  # gene features
# 
# df <- FetchData(object = so, 
#                 vars = c(dims, "ident", features), 
#                 cells = cells, 
#                 layer = "data")

# head(df)
# 
# plot_ly(
#   data = df,
#   x = ~umapunintegrated_1,
#   y = ~umapunintegrated_2,
#   z = ~umapunintegrated_3,
#   color = ~PVP01_1132700, 
#   text = ~paste("Cell:", rownames(df), "<br>Cluster:", ident),
#   type = 'scatter3d',
#   mode = 'markers',
#   marker = list(size = 5, opacity = 0.7)
# ) %>% layout(
#   title = "UMAP colored by PVP01_1132700 expression",
#   xaxis = list(title = "UMAP 1"),
#   yaxis = list(title = "UMAP 2"),
#   zaxis = list(title = "UMAP 3")
# )







# save list of data
saveRDS(save_data, "data/cleaned_dataset.rds")

# clear objects
#rm(list = ls())

