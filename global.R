# Load required libraries for global scope
library(memoise)
suppressPackageStartupMessages(library(conflicted))

# Resolve namespace conflicts BEFORE sourcing function files
suppressMessages(conflict_prefer("layout", "plotly"))

# Source helper functions
source("functions/helpers_server.R")
source("functions/helpers_UI.R")
source("functions/embedding_plots.R")

# Source modular server functions
source("functions/navigation_module.R")
source("functions/data_loading_module.R")
source("functions/metadata_filter_module.R")
source("functions/quality_control_module.R")
source("functions/gene_expression_module.R")
source("functions/embedding_visualization_module.R")

# Define a function for memoized data loading.
load_data <- memoise(function(file_path) {
  readRDS(file_path)
})

# Define paths to the data files for easy access
DATA_PATHS <- list(
  norm_df = "data/normalize_df_real.rds", 
  raw_df = "data/raw_df.rds",
  # scale_df = "data/scale_df.rds",
  cleaned_dataset = "data/cleaned_dataset.rds"
)

gff_data <- readRDS("ref/PvivaxP01_gff_data.rds")

# Pre-load the most commonly used data (normalized data and metadata)
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

# Static definitions
group_choices <- c(
  "development_phase",
  "blood_stage",
  "parasite_stage",
  "life_cycle_stage",
  "host_species",
  "treatment",
  "sample_type"
)
