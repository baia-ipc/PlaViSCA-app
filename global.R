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

