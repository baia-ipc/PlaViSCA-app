# Update large expression files with new terminology columns
# Uses cbind to avoid loading entire dataset into memory for transformation

cat("=== UPDATING LARGE FILES ===\n\n")

# Load the transformed mr_data to get the new columns
cleaned_data <- readRDS("data/cleaned_dataset.rds")
new_cols <- cleaned_data$mr_data[, c("development_phase", "blood_stage_form", "parasite_stage_overall")]

cat("New columns loaded:", ncol(new_cols), "rows:", nrow(new_cols), "\n\n")

# Function to add columns to large file
add_cols_to_large_file <- function(filename) {
    cat("Processing", filename, "...\n")
    
    # Backup
    backup_file <- file.path("data/backup_old_terminology_20260211", basename(filename))
    if (!file.exists(backup_file)) {
        cat("  Creating backup...\n")
        file.copy(filename, backup_file, overwrite = TRUE)
    }
    
    # Load file
    cat("  Loading (this may take a minute)...\n")
    df <- readRDS(filename)
    cat("  Current:", nrow(df), "rows,", ncol(df), "columns\n")
    
    # Check if already has new columns
    if ("development_phase" %in% colnames(df)) {
        cat("  ✓ Already has new columns, skipping\n\n")
        return(invisible(NULL))
    }
    
    # Add new columns  
    cat("  Adding new columns...\n")
    
    # Find where to insert (after parasite_stage or life_cycle_stage)
    meta_cols <- colnames(df)[1:60]  # Metadata is in first ~50 cols
    insert_after <- max(
        which(meta_cols == "parasite_stage"),
        which(meta_cols == "life_cycle_stage")
    )
    
    # Split dataframe
    before_cols <- df[, 1:insert_after]
    after_cols <- df[, (insert_after+1):ncol(df)]
    
    # Combine
    df_new <- cbind(before_cols, new_cols, after_cols)
    
    cat("  New:", nrow(df_new), "rows,", ncol(df_new), "columns\n")
    
    # Save
    cat("  Saving...\n")
    saveRDS(df_new, filename)
    cat("  ✓ Done\n\n")
    
    # Clean up
    rm(df, df_new, before_cols, after_cols)
    gc(verbose = FALSE)
}

# Update files
add_cols_to_large_file("data/normalize_df_real.rds")
add_cols_to_large_file("data/raw_df.rds")

cat("=== ALL FILES UPDATED ===\n")
