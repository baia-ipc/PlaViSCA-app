# Final transformation with exact terminology requirements
# Development phase: Blood stages, Liver stages, Sporozoites
# Parasite stage: Female gametocyte, Male gametocyte, Hypnozoite, Merozoite,
#                 Ring stage, Schizont (blood), Schizont (liver), Sporozoite, Trophozoite

library(dplyr)

cat("=== FINAL TERMINOLOGY TRANSFORMATION ===\n\n")

# Backup first
backup_dir <- "data/backup_before_final_terminology"
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

# Transform mr_data with EXACT new terminology
cat("Updating terminology to exact specifications...\n")
cleaned_data$mr_data <- cleaned_data$mr_data %>%
    mutate(
        # 1. Development phase (3 values)
        development_phase = case_when(
            parasite_stage == "Sporozoite stage" ~ "Sporozoites",
            parasite_stage == "Liver stage" ~ "Liver stages",
            parasite_stage == "Blood stage" ~ "Blood stages",
            TRUE ~ parasite_stage
        ),
        
        # 2. Blood-stage form (6 blood-specific values)
        blood_stage_form = case_when(
            life_cycle_stage == "Merozoite" ~ "Merozoite",
            life_cycle_stage == "Ring" ~ "Ring stage",
            life_cycle_stage == "Trophozoite" ~ "Trophozoite",
            life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood)",
            life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
            life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
            TRUE ~ NA_character_
        ),
        
        # 3. Parasite stage overall (9 values - all specific forms)
        parasite_stage_overall = case_when(
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
cat("Development phase (should be 3 values):\n")
print(table(cleaned_data$mr_data$development_phase))
cat("\nExpected: Blood stages, Liver stages, Sporozoites\n")

cat("\n\nBlood-stage form (should be 6 values + NA):\n")
print(table(cleaned_data$mr_data$blood_stage_form, useNA = "ifany"))
cat("\nExpected: Female gametocyte, Male gametocyte, Merozoite, Ring stage, Schizont (blood), Trophozoite\n")

cat("\n\nParasite stage overall (should be 9 values):\n")
print(table(cleaned_data$mr_data$parasite_stage_overall))
cat("\nExpected: Female gametocyte, Hypnozoite, Male gametocyte, Merozoite, Ring stage, Schizont (blood), Schizont (liver), Sporozoite, Trophozoite\n")

# Save cleaned_dataset
cat("\n\nSaving cleaned_dataset.rds...\n")
saveRDS(cleaned_data, "data/cleaned_dataset.rds")

# Now update the large files
cat("\nUpdating normalize_df_real.rds...\n")
norm_df <- readRDS("data/normalize_df_real.rds")
cat("  Current columns:", ncol(norm_df), "\n")

# Get the new columns from mr_data
new_cols <- cleaned_data$mr_data[, c("development_phase", "blood_stage_form", "parasite_stage_overall")]

# Find where to insert (after existing metadata, before expression data)
# First 55 columns are metadata
meta_end_idx <- 55
norm_df_meta <- norm_df[, 1:meta_end_idx]
norm_df_expr <- norm_df[, (meta_end_idx + 1):ncol(norm_df)]

# Remove old new columns if they exist
cols_to_remove <- c("development_phase", "blood_stage_form", "parasite_stage_overall")
norm_df_meta <- norm_df_meta[, !names(norm_df_meta) %in% cols_to_remove]

# Combine: metadata + new columns + expression
norm_df <- cbind(norm_df_meta, new_cols, norm_df_expr)
cat("  Updated columns:", ncol(norm_df), "\n")
saveRDS(norm_df, "data/normalize_df_real.rds")

cat("\nUpdating raw_df.rds...\n")
raw_df <- readRDS("data/raw_df.rds")
cat("  Current columns:", ncol(raw_df), "\n")

# Same process for raw_df
raw_df_meta <- raw_df[, 1:meta_end_idx]
raw_df_expr <- raw_df[, (meta_end_idx + 1):ncol(raw_df)]
raw_df_meta <- raw_df_meta[, !names(raw_df_meta) %in% cols_to_remove]
raw_df <- cbind(raw_df_meta, new_cols, raw_df_expr)
cat("  Updated columns:", ncol(raw_df), "\n")
saveRDS(raw_df, "data/raw_df.rds")

cat("\n✓ DONE!\n")
cat("\nAll files updated with final terminology.\n")
