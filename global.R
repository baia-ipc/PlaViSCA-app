# Packages of the app
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(shinycssloaders)
library(bslib)
library(fontawesome)
suppressPackageStartupMessages(library(conflicted))
suppressPackageStartupMessages(library(tidyverse))
library(later)
library(writexl)

# Resolve namespace conflicts before sourcing the function files
suppressMessages({
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  conflict_prefer("show", "methods")
  conflict_prefer("layout", "plotly")
})

# Helpers, by topic (functions/<topic>/). The data column configuration comes
# first: the other files use get_data_column(), get_data_label() and the
# orderings.
source("functions/config/data_columns.R")  # config/data_columns.yml
source("functions/config/site.R")          # config/external_links.yml, team.yml
source("functions/config/data_paths.R")    # data files (PLAVISCA_DATA_DIR)

# Data: startup checks, caches, filtering, expression, genes, tables (no
# Shiny reactivity)
source("functions/data/cache_keys.R")
source("functions/data/expression.R")
source("functions/data/checks.R")
source("functions/data/filters.R")
source("functions/data/genes.R")
source("functions/data/expression_data.R")
source("functions/data/gene_expression.R")
source("functions/data/gene_summaries.R")
source("functions/data/cell_counts.R")
source("functions/data/cell_details.R")
source("functions/data/data_sources.R")

# Plots
source("functions/plots/cell_counts.R")
source("functions/plots/qc.R")
source("functions/plots/gene_expression.R")
source("functions/plots/gene_distribution.R")
source("functions/plots/gene_summaries.R")
source("functions/plots/embedding_legend.R")
source("functions/plots/embeddings.R")
source("functions/plots/gene_colour_mix.R")
source("functions/plots/gene_colour_wheel.R")
source("functions/plots/embedding_gene_colours.R")
source("functions/plots/embedding_gene_keys.R")
source("functions/plots/embedding_figure.R")

# UI building blocks
source("functions/ui/helpers.R")
source("functions/ui/gene_expression_card.R")
source("functions/ui/plot_builder.R")

# Server code, one file per tab or shared part (called from server.R)
source("server/data_loading.R")
source("server/filter_choices.R")
source("server/shared_filters.R")
source("server/tab_plots.R")
source("server/tab_data_table.R")
source("server/tab_qc.R")
source("server/tab_data_sources.R")
source("server/tab_gene_expression.R")
source("server/gene_card.R")
source("server/tab_embeddings.R")
source("server/embedding_controls.R")
source("server/embedding_compare.R")
source("server/cell_details.R")

# Resolve all application data paths from PLAVISCA_DATA_DIR (default "data").
# Never hardcode a data path anywhere else in the app.
DATA_PATHS <- app_data_paths()

missing_files <- Filter(function(p) !file.exists(p), unlist(DATA_PATHS))
if (length(missing_files) > 0) {
  stop(sprintf(
    paste0(
      "PlaViSCA cannot start: the following data file(s) are missing from ",
      "PLAVISCA_DATA_DIR ('%s'):\n  - %s\n",
      "Set PLAVISCA_DATA_DIR to the app_data directory written by the ",
      "preprocessing pipeline."
    ),
    app_data_dir(), paste(missing_files, collapse = "\n  - ")
  ), call. = FALSE)
}

gff_data <- readRDS(DATA_PATHS$gene_annotation)

# The metadata of the cells, their embeddings and the per-study tables
cleaned_data_list <- readRDS(DATA_PATHS$cleaned_dataset)
mr_data <- cleaned_data_list$mr_data
hiv_data <- cleaned_data_list$hiv_data
pca_df <- cleaned_data_list$pca_df

rm(cleaned_data_list)

# Bibliography / data-source table, validated against the atlas at startup.
pub_data_table <- readr::read_csv(DATA_PATHS$data_source, show_col_types = FALSE)

# The normalized expression matrix, loaded and checked now (the raw one on
# first use, see functions/data/expression_data.R). The checks
# stop the app (not just warn) if the data do not match what it expects: a
# column mismatch must never turn into empty dropdowns or misleading plots.
APP_DATA_CHECKS <- load_expression_data("normalize", bibliography = pub_data_table)

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

# The user manual (built HTML), served at manual/ for the Manual tab
addResourcePath("manual", "manual/docs/build/html")

# The plot shown when the Embeddings tab opens is drawn once the app has
# started, so that the first session gets it from the cache.
later::later(warm_embedding_plot_cache, 0)
