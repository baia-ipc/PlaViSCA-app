# source function
source("functions/functions.R")

# read gff data
gff_data <- read.table("ref/PvivaxP01_gff_data.tsv", sep = "\t", header = TRUE)

# read data
list_data <- readRDS("data/cleaned_dataset.rds")
mr_data <- list_data$mr_data
pca_std_df <- list_data$pca_std_df
vf_data <- list_data$vf_data
top_genes_exp <- list_data$top_genes_exp
rm(list_data)

## Select columns for 2D plots
pc_columns <- grep("^pc_\\d+", colnames(mr_data), ignore.case = TRUE, value = TRUE)
pc_int_columns <- grep("^int.*pca.*", colnames(mr_data), ignore.case = TRUE, value = TRUE)
umap_columns <- grep("^umap_\\d+$", colnames(mr_data), ignore.case = TRUE, value = TRUE)
umap_uint_columns <- grep("^umapunint.*", colnames(mr_data), ignore.case = TRUE, value = TRUE)
tsne_columns <- grep("t_sne", colnames(mr_data), ignore.case = TRUE, value = TRUE)
cca_snn <- grep("CCA_snn", colnames(mr_data), ignore.case = TRUE, value = TRUE)
