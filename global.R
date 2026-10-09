# Load required libraries for global scope
library(memoise)
library(bslib)
library(fontawesome)
suppressPackageStartupMessages(library(conflicted))

# Resolve namespace conflicts BEFORE sourcing function files
suppressMessages(conflict_prefer("layout", "plotly"))

# Source data column config first - other function files rely on
# get_data_column()/get_data_label() and the vocabulary/ordering helpers.
source("functions/data_columns_config.R")

# External links and team information (config/external_links.yml, team.yml).
source("functions/site_config.R")

# Data file paths (PLAVISCA_DATA_DIR) and the startup data checks.
source("functions/data_paths.R")
source("functions/data_checks.R")

# Server-side helpers (no Shiny reactivity, except tab_sync.R)
source("functions/filters.R")
source("functions/genes.R")
source("functions/gene_expression_data.R")
source("functions/data_summary.R")
source("functions/tab_sync.R")

# Plots
source("functions/plots_metadata_qc.R")
source("functions/plots_gene_expression.R")
source("functions/embedding_plots.R")

# UI building blocks
source("functions/helpers_UI.R")

# Define a function for memoized data loading.
load_data <- memoise(function(file_path) {
  readRDS(file_path)
})

# Resolve all application data paths from PLAVISCA_DATA_DIR (default "data").
# Never hardcode a data path anywhere else in the app.
DATA_PATHS <- app_data_paths()

missing_files <- Filter(function(p) !file.exists(p), unlist(DATA_PATHS))
if (length(missing_files) > 0) {
  stop(sprintf(
    paste0(
      "PlaViSca cannot start: the following data file(s) are missing from ",
      "PLAVISCA_DATA_DIR ('%s'):\n  - %s\n",
      "Set PLAVISCA_DATA_DIR to the app_data directory written by the ",
      "preprocessing pipeline."
    ),
    app_data_dir(), paste(missing_files, collapse = "\n  - ")
  ), call. = FALSE)
}

gff_data <- readRDS(DATA_PATHS$gene_annotation)

# Pre-load the most commonly used data (normalized data and metadata)
norm_df <- readRDS(DATA_PATHS$norm_df)
cleaned_data_list <- readRDS(DATA_PATHS$cleaned_dataset)
mr_data <- cleaned_data_list$mr_data
hiv_data <- cleaned_data_list$hiv_data
pca_df <- cleaned_data_list$pca_df

rm(cleaned_data_list)

# Bibliography / data-source table, validated against the atlas at startup.
pub_data_table <- readr::read_csv(DATA_PATHS$data_source, show_col_types = FALSE)

# Stop (not just warn) if the data do not match what the app expects: a column
# mismatch must never turn into empty dropdowns or misleading plots.
APP_DATA_CHECKS <- stop_if_app_data_invalid(
  mr_data = mr_data,
  expr_df = norm_df,
  bibliography = pub_data_table
)

# Gene columns of the expression tables, identified by gene identifier rather
# than by column position (see identify_gene_columns()).
GENE_COLUMNS <- identify_gene_columns(norm_df, mr_data)

# Define embedding column variables based on the pre-loaded metadata
pca_u <- grep("^pca_u_\\d+", colnames(mr_data), ignore.case = TRUE, value = TRUE)
pca_i <- grep("^pca_i_\\d+", colnames(mr_data), ignore.case = TRUE, value = TRUE)
umap_i <- grep("^umap_i_\\d+", colnames(mr_data), ignore.case = TRUE, value = TRUE)
umap_u <- grep("^umap_u_\\d+", colnames(mr_data), ignore.case = TRUE, value = TRUE)
tsne <- grep("t_sne", colnames(mr_data), ignore.case = TRUE, value = TRUE)

# Static definitions - "Group by" choices resolve through the config layer.
group_choices <- unlist(field_choices(c(
  "study_label",
  "broad_stage",
  "detailed_stage",
  "host_species",
  "treatment"
)))
