# Corrected transformation with proper column names
# 1. development_phase -> development_stage
# 2. parasite_stage_overall -> parasite_stage (rename old parasite_stage to parasite_stage_old)
# 3. Keep blood_stage_form as is

library(dplyr)

cat("=== CORRECTED TERMINOLOGY TRANSFORMATION ===\n\n")

# Backup first
backup_dir <- "data/backup_before_rename_columns"
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
cat("Loading data files...\n")
cleaned_data <- readRDS("data/cleaned_dataset.rds")

cat("Current mr_data columns:", ncol(cleaned_data$mr_data), "\n")
cat("Current mr_data rows:", nrow(cleaned_data$mr_data), "\n\n")

# Check if old columns exist
cat("Checking existing columns:\n")
cat("  - development_phase:", "development_phase" %in% names(cleaned_data$mr_data), "\n")
cat("  - parasite_stage_overall:", "parasite_stage_overall" %in% names(cleaned_data$mr_data), "\n")
cat("  - parasite_stage:", "parasite_stage" %in% names(cleaned_data$mr_data), "\n")
cat("  - life_cycle_stage:", "life_cycle_stage" %in% names(cleaned_data$mr_data), "\n\n")

# Step 1: Rename old parasite_stage to parasite_stage_old (to preserve original data)
cat("Step 1: Renaming old parasite_stage to parasite_stage_old...\n")
names(cleaned_data$mr_data)[names(cleaned_data$mr_data) == "parasite_stage"] <- "parasite_stage_old"

# Step 2: Recreate columns with correct values and names
cat("Step 2: Creating new columns with corrected terminology...\n")
cleaned_data$mr_data <- cleaned_data$mr_data %>%
    mutate(
        # 1. Development stage (was development_phase)
        development_stage = case_when(
            parasite_stage_old == "Sporozoite stage" ~ "Sporozoites",
            parasite_stage_old == "Liver stage" ~ "Liver stages",
            parasite_stage_old == "Blood stage" ~ "Blood stages",
            TRUE ~ parasite_stage_old
        ),
        
        # 2. Blood stage form - same as before
        blood_stage_form = case_when(
            life_cycle_stage == "Merozoite" ~ "Merozoite",
            life_cycle_stage == "Ring" ~ "Ring stage",
            life_cycle_stage == "Trophozoite" ~ "Trophozoite",
            life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood)",
            life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
            life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
            TRUE ~ NA_character_
        ),
        
        # 3. Parasite stage (was parasite_stage_overall)
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

# Remove old development_phase and parasite_stage_overall columns if they exist
cleaned_data$mr_data <- cleaned_data$mr_data %>%
    select(-any_of(c("development_phase", "parasite_stage_overall")))

cat("Updated mr_data columns:", ncol(cleaned_data$mr_data), "\n\n")

# Verification
cat("=== VERIFICATION ===\n\n")
cat("Development stage (should be 3 values):\n")
print(table(cleaned_data$mr_data$development_stage))
cat("\nExpected: Blood stages, Liver stages, Sporozoites\n")

cat("\n\nBlood stage form (should be 6 values + NA):\n")
print(table(cleaned_data$mr_data$blood_stage_form, useNA = "ifany"))
cat("\nExpected: Female gametocyte, Male gametocyte, Merozoite, Ring stage, Schizont (blood), Trophozoite\n")
cat("Note: NAs are correct for non-blood stage cells (Sporozoites and Liver stages)\n")

cat("\n\nParasite stage (should be 9 values):\n")
print(table(cleaned_data$mr_data$parasite_stage))
cat("\nExpected: Female gametocyte, Hypnozoite, Male gametocyte, Merozoite, Ring stage, Schizont (blood), Schizont (liver), Sporozoite, Trophozoite\n")

# Save cleaned_dataset
cat("\n\nSaving cleaned_dataset.rds...\n")
saveRDS(cleaned_data, "data/cleaned_dataset.rds")

# Now update the large files
cat("\nUpdating normalize_df_real.rds...\n")
norm_df <- readRDS("data/normalize_df_real.rds")
cat("  Current columns:", ncol(norm_df), "\n")

# Get the new columns from mr_data
new_cols <- cleaned_data$mr_data[, c("development_stage", "blood_stage_form", "parasite_stage")]

# Find metadata columns (first 55 or so)
# Look for where gene expression starts (column names starting with PVP01)
gene_start_idx <- min(which(grepl("^PVP01", names(norm_df))))
meta_end_idx <- gene_start_idx - 1

cat("  Metadata ends at column:", meta_end_idx, "\n")

norm_df_meta <- norm_df[, 1:meta_end_idx]
norm_df_expr <- norm_df[, (meta_end_idx + 1):ncol(norm_df)]

# Remove old columns
cols_to_remove <- c("development_phase", "parasite_stage_overall", "development_stage", "blood_stage_form", "parasite_stage")
norm_df_meta <- norm_df_meta[, !names(norm_df_meta) %in% cols_to_remove]

# Combine: metadata + new columns + expression
norm_df <- cbind(norm_df_meta, new_cols, norm_df_expr)
cat("  Updated columns:", ncol(norm_df), "\n")
saveRDS(norm_df, "data/normalize_df_real.rds")

cat("\nUpdating raw_df.rds...\n")
raw_df <- readRDS("data/raw_df.rds")
cat("  Current columns:", ncol(raw_df), "\n")

# Same process for raw_df
gene_start_idx_raw <- min(which(grepl("^PVP01", names(raw_df))))
meta_end_idx_raw <- gene_start_idx_raw - 1

raw_df_meta <- raw_df[, 1:meta_end_idx_raw]
raw_df_expr <- raw_df[, (meta_end_idx_raw + 1):ncol(raw_df)]
raw_df_meta <- raw_df_meta[, !names(raw_df_meta) %in% cols_to_remove]
raw_df <- cbind(raw_df_meta, new_cols, raw_df_expr)
cat("  Updated columns:", ncol(raw_df), "\n")
saveRDS(raw_df, "data/raw_df.rds")

cat("\n✓ DONE!\n")
cat("\nColumn name changes:\n")
cat("  - development_phase -> development_stage\n")
cat("  - parasite_stage_overall -> parasite_stage\n")
cat("  - old parasite_stage -> parasite_stage_old (preserved)\n")
cat("\nAll files updated.\n")
