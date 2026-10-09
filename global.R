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

# Data-contract validation + data path resolution (PLAVISCA_DATA_DIR).
source("functions/data_contract.R")

# Source helper functions
source("functions/helpers_server.R")
source("functions/helpers_UI.R")
source("functions/embedding_plots.R")

# NOTE (Phase 3): the former functions/*_module.R files were duplicate,
# never-called implementations of logic that server.R defines locally, and
# several of them still referenced pre-Phase-2 metadata columns. They have
# been moved to docs/legacy/ and are no longer sourced. See
# audit/phase3_app_repair/REPORT.md.

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
      "Set PLAVISCA_DATA_DIR to a directory containing the Phase-2 atlas export."
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

# Fail fast (hard stop, not a warning) if the data does not satisfy the app's
# contract - a schema mismatch must never degrade into empty dropdowns or
# misleading plots.
APP_DATA_CONTRACT <- enforce_app_data_contract(
  mr_data = mr_data,
  expr_df = norm_df,
  bibliography = pub_data_table
)

# Gene columns of the expression tables, identified by gene identifier rather
# than by a positional column range (the metadata block grew from ~52 to 88
# columns in Phase 2; any positional assumption would misread metadata as
# genes).
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
