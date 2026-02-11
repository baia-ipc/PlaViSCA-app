# ============================================================================
# ARCHIVE: TRANSFORMATION REFERENCE SCRIPT
# ============================================================================
# 
# PURPOSE:
# This script is kept as a REFERENCE for the transformation logic that was used
# to create the 3-tier terminology system (development_phase, blood_stage, 
# parasite_stage) from the legacy life_cycle_stage column.
#
# IMPORTANT: DO NOT RUN THIS SCRIPT!
# This transformation logic has been integrated directly into the preprocessing
# pipeline (pre_process_data/scripts/singleR.R). Future preprocessing runs will
# automatically create the correct columns without needing this post-processing.
#
# HISTORY:
# - Previously, individual study scripts created a "parasite_stage" column with
#   values like "Blood stages", "Liver stages", "Sporozoites" (plural forms)
# - This led to data inconsistency: 2,857 Hazzard2022 sporozoite cells had
#   parasite_stage_old="Blood stage" but life_cycle_stage="Sporozoite"
# - This script fixed the issue by using life_cycle_stage (from SingleR) as
#   the authoritative source of truth instead of parasite_stage_old
#
# CURRENT STATE (as of Feb 2026):
# - singleR.R now derives the 3 new columns directly from life_cycle_stage
# - Individual study scripts NO LONGER create parasite_stage
# - This script is archived for reference only
#
# SEE ALSO:
# - pre_process_data/scripts/singleR.R (lines 414-471)
# - AGENTS.md (preprocessing documentation)
#
# ============================================================================

# CORRECTED transformation - use life_cycle_stage as source of truth
# This fixes the issue where 2,857 Sporozoites were classified as "Blood stages"

library(dplyr)

cat("=== CORRECTED TRANSFORMATION (using life_cycle_stage) ===\n\n")

# Backup first
backup_dir <- "data/backup_before_corrected_fix"
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

# Load data
cat("Loading cleaned_dataset.rds...\n")
cleaned_data <- readRDS("data/cleaned_dataset.rds")

cat("Current mr_data columns:", ncol(cleaned_data$mr_data), "\n")
cat("Current mr_data rows:", nrow(cleaned_data$mr_data), "\n\n")

# Create columns using life_cycle_stage as PRIMARY source of truth
cat("Creating columns using life_cycle_stage as source of truth...\n")
cleaned_data$mr_data <- cleaned_data$mr_data %>%
    mutate(
        # 1. Development phase - derived from life_cycle_stage
        development_phase = case_when(
            life_cycle_stage == "Sporozoite" ~ "Sporozoites",
            life_cycle_stage == "Hypnozoite" ~ "Liver stages",
            life_cycle_stage == "Schizont (liver stage)" ~ "Liver stages",
            # Everything else is blood stage
            TRUE ~ "Blood stages"
        ),
        
        # 2. Blood stage - only for blood stage cells
        blood_stage = case_when(
            life_cycle_stage == "Merozoite" ~ "Merozoite",
            life_cycle_stage == "Ring" ~ "Ring stage",
            life_cycle_stage == "Trophozoite" ~ "Trophozoite",
            life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood)",
            life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
            life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
            TRUE ~ NA_character_
        ),
        
        # 3. Parasite stage - just clean up life_cycle_stage names
        parasite_stage = case_when(
            life_cycle_stage == "Sporozoite" ~ "Sporozoite",
            life_cycle_stage == "Merozoite" ~ "Merozoite",
            life_cycle_stage == "Ring" ~ "Ring stage",
            life_cycle_stage == "Trophozoite" ~ "Trophozoite",
            life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood)",
            life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
            life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
            life_cycle_stage == "Hypnozoite" ~ "Hypnozoite",
            life_cycle_stage == "Schizont (liver stage)" ~ "Schizont (liver)",
            TRUE ~ life_cycle_stage
        )
    )

cat("Updated mr_data columns:", ncol(cleaned_data$mr_data), "\n\n")

# Verification
cat("=== VERIFICATION ===\n\n")
cat("Development phase (3 values):\n")
print(table(cleaned_data$mr_data$development_phase))

cat("\n\nBlood stage (6 values + NA):\n")
blood_table <- table(cleaned_data$mr_data$blood_stage, useNA = "ifany")
print(blood_table)
cat("\nNA count:", sum(is.na(cleaned_data$mr_data$blood_stage)), "\n")
cat("Expected NAs: Sporozoites + Liver stages = ", 
    sum(cleaned_data$mr_data$development_phase %in% c("Sporozoites", "Liver stages")), "\n")

cat("\n\nParasite stage (9 values):\n")
print(table(cleaned_data$mr_data$parasite_stage))

# Check the problematic cells
cat("\n\n=== CHECKING PREVIOUSLY PROBLEMATIC CELLS ===\n")
problem_cells <- cleaned_data$mr_data[cleaned_data$mr_data$life_cycle_stage == "Sporozoite" & 
                                      cleaned_data$mr_data$parasite_stage_old == "Blood stage", ]
cat("Cells with life_cycle_stage=Sporozoite & parasite_stage_old=Blood stage:", nrow(problem_cells), "\n")
if (nrow(problem_cells) > 0) {
  cat("Their development_phase values:\n")
  print(table(problem_cells$development_phase))
  cat("\nShould ALL be 'Sporozoites' now!\n")
}

# Save cleaned_dataset
cat("\n\nSaving cleaned_dataset.rds...\n")
saveRDS(cleaned_data, "data/cleaned_dataset.rds")

# Update normalize_df_real.rds
cat("\nUpdating normalize_df_real.rds...\n")
norm_df <- readRDS("data/normalize_df_real.rds")
cat("  Current columns:", ncol(norm_df), "\n")

new_cols <- cleaned_data$mr_data[, c("development_phase", "blood_stage", "parasite_stage")]
gene_start_idx <- min(which(grepl("^PVP01", names(norm_df))))
meta_end_idx <- gene_start_idx - 1

norm_df_meta <- norm_df[, 1:meta_end_idx]
norm_df_expr <- norm_df[, (meta_end_idx + 1):ncol(norm_df)]
cols_to_remove <- c("development_phase", "blood_stage", "parasite_stage")
norm_df_meta <- norm_df_meta[, !names(norm_df_meta) %in% cols_to_remove]
norm_df <- cbind(norm_df_meta, new_cols, norm_df_expr)
cat("  Updated columns:", ncol(norm_df), "\n")
saveRDS(norm_df, "data/normalize_df_real.rds")

# Update raw_df.rds
cat("\nUpdating raw_df.rds...\n")
raw_df <- readRDS("data/raw_df.rds")
cat("  Current columns:", ncol(raw_df), "\n")

gene_start_idx_raw <- min(which(grepl("^PVP01", names(raw_df))))
meta_end_idx_raw <- gene_start_idx_raw - 1
raw_df_meta <- raw_df[, 1:meta_end_idx_raw]
raw_df_expr <- raw_df[, (meta_end_idx_raw + 1):ncol(raw_df)]
raw_df_meta <- raw_df_meta[, !names(raw_df_meta) %in% cols_to_remove]
raw_df <- cbind(raw_df_meta, new_cols, raw_df_expr)
cat("  Updated columns:", ncol(raw_df), "\n")
saveRDS(raw_df, "data/raw_df.rds")

cat("\n✓ DONE!\n")
cat("\nFix applied: Used life_cycle_stage (not parasite_stage_old) as source of truth.\n")
cat("Result: All 2,857 Sporozoite cells now correctly have development_phase='Sporozoites'\n")
