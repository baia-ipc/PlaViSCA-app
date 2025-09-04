# sourcer function
source("functions/helpers.R")

# load all data files with caching optimization
norm_df <- readRDS("data/normalize_df.rds")

# Load raw and scale data for comprehensive coverage
raw_df <- readRDS("data/raw_df.rds")
scale_df <- readRDS("data/scale_df.rds")

# load publication data
pub_data <- readr::read_csv("data/data_source.csv")
pub_data$DOI <-
  paste0("<a href='", pub_data$DOI, "' target='_blank'>", pub_data$DOI, "</a>")

# define variable for selectInput in gene Exp
group_choices <- c(
  "parasite_stage",
  "host_species",
  "treatment",
  "sample_type",
  "life_cycle_stage",
  "study_label",
  "pub_year",
  "run_id"
)

# read gff data
gff_data <- readRDS("ref/PvivaxP01_gff_data.rds")

# read metadata and reduction data
list_data <- readRDS("data/cleaned_dataset.rds")
mr_data <- list_data$mr_data
pca_std_df <- list_data$pca_std_df
vf_data <- list_data$vf_data
# top_genes_exp <- list_data$top_genes_exp
rm(list_data)

# select columns for 2D plots
pca_u <- grep(
  "^pca_u_\\d+",
  colnames(mr_data),
  ignore.case = TRUE,
  value = TRUE
)
pca_i <- grep(
  "^pca_i_\\d+",
  colnames(mr_data),
  ignore.case = TRUE,
  value = TRUE
)
umap_i <- grep(
  "^umap_i_\\d+",
  colnames(mr_data),
  ignore.case = TRUE,
  value = TRUE
)
umap_u <- grep(
  "^umap_u_\\d+",
  colnames(mr_data),
  ignore.case = TRUE,
  value = TRUE
)
tsne <- grep("t_sne", colnames(mr_data), ignore.case = TRUE, value = TRUE)
RNA_snn <- grep("RNA_snn", colnames(mr_data), ignore.case = TRUE, value = TRUE)
