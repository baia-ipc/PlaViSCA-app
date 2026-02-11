# Clean up obsolete columns from PlaViSCA data files
# This script removes unused metadata columns to reduce file size and complexity

library(dplyr)

cat("=== CLEANING OBSOLETE COLUMNS FROM DATA FILES ===\n\n")

# Backup first
backup_dir <- "data/backup_before_column_cleanup"
dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)

cat("Creating backup...\n")
file.copy("data/cleaned_dataset.rds", 
          file.path(backup_dir, "cleaned_dataset.rds"),
          overwrite = TRUE)
file.copy("data/normalize_df_real.rds",
          file.path(backup_dir, "normalize_df_real.rds"),
          overwrite = TRUE)
file.copy("data/raw_df.rds",
          file.path(backup_dir, "raw_df.rds"),
          overwrite = TRUE)

# Define columns to KEEP
keep_metadata_cols <- c(
  # Identifiers
  "barcode",
  
  # Stage columns (NEW SYSTEM)
  "development_phase", "blood_stage", "parasite_stage",
  
  # Stage columns (OLD - keep for backward compatibility)
  "life_cycle_stage",
  
  # Essential metadata
  "study_label", "sample_type", "host_species", "strain",
  "treatment", "geographic_location", "sc_technology", 
  "sequencer", "hour_post_invasion", "day_post_infection",
  
  # QC metrics
  "n_count_rna", "n_feature_rna",
  
  # Embeddings (will be identified by pattern)
  # pca_u_1, pca_u_2, pca_u_3, pca_i_1, pca_i_2, pca_i_3,
  # umap_u_1, umap_u_2, umap_u_3, umap_i_1, umap_i_2, umap_i_3,
  # t_sne_1, t_sne_2, t_sne_3
  
  # Gene expression columns (PVP01_*) will be identified by pattern
)

# Columns to explicitly REMOVE
remove_cols <- c(
  "parasite_stage_old",    # Legacy preprocessing metadata
  "liver_form",            # Not used
  "pred_gametocyte",       # Not used
  "seurat_clusters",       # Not used
  "refine_state",          # Not used (info already in life_cycle_stage)
  "goegraphic_location",   # Typo duplicate of geographic_location
  "sequncer",              # Typo duplicate of sequencer
  "pub_year",              # Not displayed
  "study_pmid",            # Not displayed
  "run_id",                # Not displayed
  "num_srr",               # Not displayed
  "host_id",               # Not displayed
  "biological_replicate"   # Not displayed
)

cat("\nColumns to remove:", length(remove_cols), "\n")
cat(paste0("  - ", remove_cols, "\n"))

# =============================================================================
# CLEAN CLEANED_DATASET.RDS
# =============================================================================
cat("\n\n=== Processing cleaned_dataset.rds ===\n")
cleaned_data <- readRDS("data/cleaned_dataset.rds")

cat("Before: mr_data has", ncol(cleaned_data$mr_data), "columns\n")
cat("Before: mr_data has", nrow(cleaned_data$mr_data), "rows\n")

# Get column names
all_cols <- names(cleaned_data$mr_data)

# Identify embedding columns
embedding_cols <- grep("^(pca_[ui]_[123]|umap_[ui]_[123]|t_sne_[123])$", all_cols, value = TRUE)

# Identify gene columns
gene_cols <- grep("^PVP01", all_cols, value = TRUE)

# Combine all columns to keep
cols_to_keep <- c(
  keep_metadata_cols[keep_metadata_cols %in% all_cols],
  embedding_cols,
  gene_cols
)

# Verify we're removing the right columns
cols_being_removed <- setdiff(all_cols, cols_to_keep)
cat("\nColumns being removed from mr_data:\n")
cat(paste0("  - ", cols_being_removed, "\n"))

# Remove obsolete columns
cleaned_data$mr_data <- cleaned_data$mr_data[, cols_to_keep]

cat("\nAfter: mr_data has", ncol(cleaned_data$mr_data), "columns\n")
cat("After: mr_data has", nrow(cleaned_data$mr_data), "rows\n")

# Save
cat("Saving cleaned_dataset.rds...\n")
saveRDS(cleaned_data, "data/cleaned_dataset.rds")

# =============================================================================
# CLEAN NORMALIZE_DF_REAL.RDS
# =============================================================================
cat("\n\n=== Processing normalize_df_real.rds ===\n")
norm_df <- readRDS("data/normalize_df_real.rds")

cat("Before:", ncol(norm_df), "columns\n")
cat("Before:", nrow(norm_df), "rows\n")

# Get column names
all_cols_norm <- names(norm_df)

# Find where gene columns start
gene_start_idx <- min(which(grepl("^PVP01", all_cols_norm)))
meta_cols_norm <- all_cols_norm[1:(gene_start_idx - 1)]
gene_cols_norm <- all_cols_norm[gene_start_idx:length(all_cols_norm)]

cat("\nMetadata columns:", length(meta_cols_norm), "\n")
cat("Gene columns:", length(gene_cols_norm), "\n")

# Identify embedding columns in metadata
embedding_cols_norm <- grep("^(pca_[ui]_[123]|umap_[ui]_[123]|t_sne_[123])$", meta_cols_norm, value = TRUE)

# Columns to keep from metadata
meta_to_keep <- c(
  keep_metadata_cols[keep_metadata_cols %in% meta_cols_norm],
  embedding_cols_norm
)

cols_being_removed_norm <- setdiff(meta_cols_norm, meta_to_keep)
cat("\nMetadata columns being removed:\n")
cat(paste0("  - ", cols_being_removed_norm, "\n"))

# Keep: selected metadata + all gene columns
norm_df <- norm_df[, c(meta_to_keep, gene_cols_norm)]

cat("\nAfter:", ncol(norm_df), "columns\n")
cat("After:", nrow(norm_df), "rows\n")

cat("Saving normalize_df_real.rds...\n")
saveRDS(norm_df, "data/normalize_df_real.rds")

# =============================================================================
# CLEAN RAW_DF.RDS
# =============================================================================
cat("\n\n=== Processing raw_df.rds ===\n")
raw_df <- readRDS("data/raw_df.rds")

cat("Before:", ncol(raw_df), "columns\n")
cat("Before:", nrow(raw_df), "rows\n")

# Get column names
all_cols_raw <- names(raw_df)

# Find where gene columns start
gene_start_idx_raw <- min(which(grepl("^PVP01", all_cols_raw)))
meta_cols_raw <- all_cols_raw[1:(gene_start_idx_raw - 1)]
gene_cols_raw <- all_cols_raw[gene_start_idx_raw:length(all_cols_raw)]

cat("\nMetadata columns:", length(meta_cols_raw), "\n")
cat("Gene columns:", length(gene_cols_raw), "\n")

# Identify embedding columns in metadata
embedding_cols_raw <- grep("^(pca_[ui]_[123]|umap_[ui]_[123]|t_sne_[123])$", meta_cols_raw, value = TRUE)

# Columns to keep from metadata
meta_to_keep_raw <- c(
  keep_metadata_cols[keep_metadata_cols %in% meta_cols_raw],
  embedding_cols_raw
)

cols_being_removed_raw <- setdiff(meta_cols_raw, meta_to_keep_raw)
cat("\nMetadata columns being removed:\n")
cat(paste0("  - ", cols_being_removed_raw, "\n"))

# Keep: selected metadata + all gene columns
raw_df <- raw_df[, c(meta_to_keep_raw, gene_cols_raw)]

cat("\nAfter:", ncol(raw_df), "columns\n")
cat("After:", nrow(raw_df), "rows\n")

cat("Saving raw_df.rds...\n")
saveRDS(raw_df, "data/raw_df.rds")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n=== CLEANUP COMPLETE ===\n")
cat("\nBackup location:", backup_dir, "\n")
cat("\nColumns removed:\n")
cat(paste0("  - ", remove_cols, "\n"))
cat("\nAll three data files have been cleaned and saved.\n")
