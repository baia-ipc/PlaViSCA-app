#!/usr/bin/env Rscript
# PlaViSCA test runner.
# =============================================================================
#   Rscript tests/run_tests.R [output.tsv]
#
# Point it at a data directory with PLAVISCA_DATA_DIR, e.g.
#   PLAVISCA_DATA_DIR=/path/to/app_data Rscript --vanilla tests/run_tests.R
#
# Exit status is 1 if any test failed.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
out_tsv <- if (length(args) >= 1) args[[1]] else NA_character_

app_root <- Sys.getenv("PLAVISCA_APP_ROOT", unset = getwd())
if (!file.exists(file.path(app_root, "config", "data_columns.yml"))) {
    stop("Run from the PlaViSCA app root, or set PLAVISCA_APP_ROOT.", call. = FALSE)
}
setwd(app_root)
Sys.setenv(PLAVISCA_APP_ROOT = normalizePath(app_root))

source("tests/helper.R")
load_app_config()

# The filter and plot helpers are needed by a couple of behavioural tests;
# they pull in dplyr/plotly, so load them best-effort and let dependent tests
# skip if absent.
suppressWarnings(try({
    suppressPackageStartupMessages({
        library(dplyr)
        library(plotly)
    })
    source("functions/data/cache_keys.R")
    source("functions/data/filters.R")
    source("functions/data/genes.R")
    source("functions/data/gene_expression.R")
    source("functions/plots/qc.R")
    source("functions/data/cell_counts.R")
    source("functions/plots/cell_counts.R")
    source("functions/data/gene_summaries.R")
    source("functions/plots/gene_expression.R")
    source("functions/plots/gene_distribution.R")
    source("functions/plots/gene_summaries.R")
    source("functions/plots/embedding_legend.R")
    source("functions/plots/embeddings.R")
    source("functions/plots/embedding_hover.R")
    source("functions/plots/gene_colour_mix.R")
    source("functions/plots/gene_colour_wheel.R")
    source("functions/plots/embedding_gene_colours.R")
    source("functions/plots/embedding_gene_keys.R")
    source("functions/plots/embedding_figure.R")
    source("functions/data/cell_details.R")
}, silent = TRUE))

test_files <- c(
    "tests/test_data_checks.R",
    "tests/test_config_fields.R",
    "tests/test_lifecycle.R",
    "tests/test_idc_and_gametocytes.R",
    "tests/test_bibliography_and_filters.R",
    "tests/test_expression.R",
    "tests/test_cell_count_plots.R",
    "tests/test_gene_plots.R",
    "tests/test_embedding_gene_colours.R",
    "tests/test_embedding_legend.R",
    "tests/test_embedding_hover.R",
    "tests/test_app_code.R",
    "tests/test_server.R"
)

cat(sprintf("PlaViSCA test suite\n  app root: %s\n  data dir: %s\n\n",
    app_root, app_data_dir()))

for (f in test_files) {
    if (!file.exists(f)) next
    cat(sprintf("== %s\n", basename(f)))
    .TEST_RESULTS$current_file <- basename(f)
    source(f, local = new.env(parent = globalenv()))
    cat("\n")
}

results <- do.call(rbind, .TEST_RESULTS$rows)
n_pass <- sum(results$status == "PASS")
n_fail <- sum(results$status == "FAIL")
n_skip <- sum(results$status == "SKIP")

cat(sprintf(
    "-----------------------------------------------\nTOTAL: %d   PASS: %d   FAIL: %d   SKIP: %d\n",
    nrow(results), n_pass, n_fail, n_skip
))

if (!is.na(out_tsv)) {
    dir.create(dirname(out_tsv), showWarnings = FALSE, recursive = TRUE)
    utils::write.table(results, out_tsv, sep = "\t", row.names = FALSE, quote = FALSE)
    cat(sprintf("Results written to %s\n", out_tsv))
}

if (n_fail > 0) quit(status = 1, save = "no")
