# sourcer function
source("functions/helpers.R")
library(memoise)

# Define a function for memoized data loading.
# This will cache the result of readRDS, so each file is only read once per session.
load_data <- memoise(function(file_path) {
  readRDS(file_path)
})

# Define paths to the data files for easy access
DATA_PATHS <- list(
  norm_df = "data/normalize_df.rds",
  raw_df = "data/raw_df.rds",
  # scale_df = "data/scale_df.rds",
  cleaned_dataset = "data/cleaned_dataset.rds"
)

gff_data <- readRDS("ref/PvivaxP01_gff_data.rds")

# Pre-load the most commonly used data (normalized data and metadata)
# This ensures immediate app responsiveness for the default user workflow
norm_df <- readRDS(DATA_PATHS$norm_df)
cleaned_data_list <- readRDS(DATA_PATHS$cleaned_dataset)
mr_data <- cleaned_data_list$mr_data
hiv_data <- cleaned_data_list$hiv_data
pca_df <- cleaned_data_list$pca_df

rm(cleaned_data_list)

# Define column variables based on the pre-loaded metadata
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

# Static definitions
group_choices <- c(
  "life_cycle_stage",
  "parasite_stage",
  "host_species",
  "treatment",
  "sample_type"
)
