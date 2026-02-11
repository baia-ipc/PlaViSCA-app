# Script to transform parasite stage terminology in existing data
# This updates the RDS files to match the new nomenclature

library(dplyr)

cat("=== TRANSFORMING PARASITE STAGE TERMINOLOGY ===\n\n")

# Load existing data
cat("Loading data files...\n")
norm_df <- readRDS("data/normalize_df_real.rds")
raw_df <- readRDS("data/raw_df.rds")
cleaned_data <- readRDS("data/cleaned_dataset.rds")

# Function to add new terminology columns
add_new_terminology <- function(df) {
    df <- df %>%
        mutate(
            # 1. Development phase (broad grouping)
            development_phase = case_when(
                parasite_stage == "Sporozoite stage" ~ "Sporozoite",
                parasite_stage == "Liver stage" ~ "Liver stages",
                parasite_stage == "Blood stage" ~ "Blood stage",
                TRUE ~ parasite_stage
            ),
            
            # 2. Blood-stage form (only for blood stage cells)
            blood_stage_form = case_when(
                life_cycle_stage == "Merozoite" ~ "Merozoites",
                life_cycle_stage == "Ring" ~ "Ring",
                life_cycle_stage == "Trophozoite" ~ "Trophozoite",
                life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood stage)",
                life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
                life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
                TRUE ~ NA_character_  # Liver and sporozoite stages don't have blood-stage forms
            ),
            
            # 3. Parasite stage (overall - specific forms, excluding liver-specific)
            parasite_stage_overall = case_when(
                life_cycle_stage == "Sporozoite" ~ "Sporozoite",
                life_cycle_stage == "Merozoite" ~ "Merozoites",
                life_cycle_stage == "Ring" ~ "Ring",
                life_cycle_stage == "Trophozoite" ~ "Trophozoite",
                life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood stage)",
                life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
                life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
                life_cycle_stage == "Hypnozoite" ~ "Hypnozoite (liver)",
                life_cycle_stage == "Schizont (liver stage)" ~ "Schizont (liver stage)",
                TRUE ~ life_cycle_stage
            )
        )
    
    # Reorder columns to place new columns after the old ones
    old_stage_col_idx <- which(names(df) == "parasite_stage")
    life_cycle_col_idx <- which(names(df) == "life_cycle_stage")
    insert_pos <- max(old_stage_col_idx, life_cycle_col_idx) + 1
    
    # Get column names
    all_cols <- names(df)
    new_cols <- c("development_phase", "blood_stage_form", "parasite_stage_overall")
    other_cols <- setdiff(all_cols, new_cols)
    
    # Reorder
    if (insert_pos <= length(other_cols)) {
        final_order <- c(
            other_cols[1:(insert_pos-1)],
            new_cols,
            other_cols[insert_pos:length(other_cols)]
        )
    } else {
        final_order <- c(other_cols, new_cols)
    }
    
    df <- df[, final_order]
    
    return(df)
}

# Transform the data
cat("\nTransforming normalize_df_real.rds...\n")
norm_df_new <- add_new_terminology(norm_df)

cat("Transforming raw_df.rds...\n")
raw_df_new <- add_new_terminology(raw_df)

cat("Transforming mr_data in cleaned_dataset.rds...\n")
cleaned_data$mr_data <- add_new_terminology(cleaned_data$mr_data)

# Verify the transformation
cat("\n=== VERIFICATION ===\n\n")

cat("Development phase values:\n")
print(table(cleaned_data$mr_data$development_phase))

cat("\n\nBlood-stage form values:\n")
print(table(cleaned_data$mr_data$blood_stage_form, useNA = "ifany"))

cat("\n\nParasite stage overall values:\n")
print(table(cleaned_data$mr_data$parasite_stage_overall))

cat("\n\nCross-tabulation (Development phase vs Blood-stage form):\n")
print(table(
    Development = cleaned_data$mr_data$development_phase,
    BloodForm = cleaned_data$mr_data$blood_stage_form,
    useNA = "ifany"
))

# Backup old files
cat("\n\n=== CREATING BACKUPS ===\n")
backup_dir <- "data/backup_old_terminology_20260211"
dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)

if (file.exists("data/normalize_df_real.rds")) {
    file.copy("data/normalize_df_real.rds", 
              file.path(backup_dir, "normalize_df_real.rds"),
              overwrite = TRUE)
    cat("✓ Backed up normalize_df_real.rds\n")
}

if (file.exists("data/raw_df.rds")) {
    file.copy("data/raw_df.rds",
              file.path(backup_dir, "raw_df.rds"),
              overwrite = TRUE)
    cat("✓ Backed up raw_df.rds\n")
}

if (file.exists("data/cleaned_dataset.rds")) {
    file.copy("data/cleaned_dataset.rds",
              file.path(backup_dir, "cleaned_dataset.rds"),
              overwrite = TRUE)
    cat("✓ Backed up cleaned_dataset.rds\n")
}

# Save transformed data
cat("\n=== SAVING TRANSFORMED DATA ===\n")

saveRDS(norm_df_new, "data/normalize_df_real.rds")
cat("✓ Saved normalize_df_real.rds\n")

saveRDS(raw_df_new, "data/raw_df.rds")
cat("✓ Saved raw_df.rds\n")

saveRDS(cleaned_data, "data/cleaned_dataset.rds")
cat("✓ Saved cleaned_dataset.rds\n")

cat("\n=== TRANSFORMATION COMPLETE ===\n")
cat("New columns added:\n")
cat("  1. development_phase (3 values)\n")
cat("  2. blood_stage_form (6 values + NA)\n")
cat("  3. parasite_stage_overall (9 values)\n")
cat("\nOld columns preserved:\n")
cat("  - parasite_stage (original)\n")
cat("  - life_cycle_stage (original)\n")
cat("\nBackups saved to:", backup_dir, "\n")
