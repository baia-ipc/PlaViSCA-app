# Where the app finds its data files.
# =============================================================================
# All data paths are built here from the PLAVISCA_DATA_DIR environment
# variable (default "data"), so the app can be pointed at any data directory
# without editing code. No other file in the app writes a data path.
# =============================================================================

#' Directory holding the app's data files.
app_data_dir <- function() {
    Sys.getenv("PLAVISCA_DATA_DIR", unset = "data")
}

#' Path of one data file inside app_data_dir().
app_data_path <- function(filename) {
    file.path(app_data_dir(), filename)
}

APP_DATA_FILES <- list(
    cleaned_dataset = "cleaned_dataset.rds",
    gene_annotation = "gene_annotation.rds",
    norm_df = "normalize_df.rds",
    raw_df = "raw_df.rds",
    data_source = "data_source.csv"
)

#' Named list of the paths of all data files used by the app.
app_data_paths <- function() {
    lapply(APP_DATA_FILES, app_data_path)
}
