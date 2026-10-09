#!/usr/bin/env Rscript
# PlaViSca test runner.
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
    stop("Run from the PlaViSca app root, or set PLAVISCA_APP_ROOT.", call. = FALSE)
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
    source("functions/filters.R")
    source("functions/genes.R")
    source("functions/gene_expression_data.R")
    source("functions/plots_metadata_qc.R")
    source("functions/embedding_plots.R")
}, silent = TRUE))

test_files <- c(
    "tests/test_data_checks.R",
    "tests/test_config_fields.R",
    "tests/test_lifecycle.R",
    "tests/test_idc_and_gametocytes.R",
    "tests/test_bibliography_and_filters.R",
    "tests/test_expression.R",
    "tests/test_app_code.R",
    "tests/test_expected_cells.R"
)

cat(sprintf("PlaViSca test suite\n  app root: %s\n  data dir: %s\n\n",
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
