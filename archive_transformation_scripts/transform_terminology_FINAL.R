# FINAL transformation with correct column names
# 1. development_stage -> development_phase
# 2. blood_stage_form -> blood_stage
# 3. parasite_stage_overall -> parasite_stage

library(dplyr)

cat("=== FINAL COLUMN NAME CORRECTIONS ===\n\n")

# Backup first
backup_dir <- "data/backup_before_final_names"
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

# Rename old parasite_stage to parasite_stage_old if it exists
if ("parasite_stage" %in% names(cleaned_data$mr_data) && 
    !"parasite_stage_old" %in% names(cleaned_data$mr_data)) {
  cat("Renaming old parasite_stage to parasite_stage_old...\n")
  names(cleaned_data$mr_data)[names(cleaned_data$mr_data) == "parasite_stage"] <- "parasite_stage_old"
}

# Create columns with exact names and values
cat("Creating columns with correct names and values...\n")
cleaned_data$mr_data <- cleaned_data$mr_data %>%
    mutate(
        # 1. Development phase (3 values)
        development_phase = case_when(
            parasite_stage_old == "Sporozoite stage" ~ "Sporozoites",
            parasite_stage_old == "Liver stage" ~ "Liver stages",
            parasite_stage_old == "Blood stage" ~ "Blood stages",
            TRUE ~ parasite_stage_old
        ),
        
        # 2. Blood stage (6 blood-specific values)
        blood_stage = case_when(
            life_cycle_stage == "Merozoite" ~ "Merozoite",
            life_cycle_stage == "Ring" ~ "Ring stage",
            life_cycle_stage == "Trophozoite" ~ "Trophozoite",
            life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood)",
            life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
            life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
            TRUE ~ NA_character_
        ),
        
        # 3. Parasite stage (9 values)
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

# Remove old columns if they exist
cleaned_data$mr_data <- cleaned_data$mr_data %>%
    select(-any_of(c("development_stage", "blood_stage_form", "parasite_stage_overall")))

cat("Updated mr_data columns:", ncol(cleaned_data$mr_data), "\n\n")

# Verification
cat("=== VERIFICATION ===\n\n")
cat("Development phase (3 values):\n")
print(table(cleaned_data$mr_data$development_phase))

cat("\n\nBlood stage (6 values + NA):\n")
print(table(cleaned_data$mr_data$blood_stage, useNA = "ifany"))

cat("\n\nParasite stage (9 values):\n")
print(table(cleaned_data$mr_data$parasite_stage))

# Save cleaned_dataset
cat("\n\nSaving cleaned_dataset.rds...\n")
saveRDS(cleaned_data, "data/cleaned_dataset.rds")

# Update normalize_df_real.rds
cat("\nUpdating normalize_df_real.rds...\n")
norm_df <- readRDS("data/normalize_df_real.rds")
cat("  Current columns:", ncol(norm_df), "\n")

# Get new columns from mr_data
new_cols <- cleaned_data$mr_data[, c("development_phase", "blood_stage", "parasite_stage")]

# Find where expression data starts
gene_start_idx <- min(which(grepl("^PVP01", names(norm_df))))
meta_end_idx <- gene_start_idx - 1
cat("  Metadata ends at column:", meta_end_idx, "\n")

norm_df_meta <- norm_df[, 1:meta_end_idx]
norm_df_expr <- norm_df[, (meta_end_idx + 1):ncol(norm_df)]

# Remove old column names
cols_to_remove <- c("development_phase", "development_stage", "blood_stage", "blood_stage_form", 
                    "parasite_stage", "parasite_stage_overall")
norm_df_meta <- norm_df_meta[, !names(norm_df_meta) %in% cols_to_remove]

# Combine
norm_df <- cbind(norm_df_meta, new_cols, norm_df_expr)
cat("  Updated columns:", ncol(norm_df), "\n")
saveRDS(norm_df, "data/normalize_df_real.rds")

# Update raw_df.rds
cat("\nUpdating raw_df.rds...\n")
raw_df <- readRDS("data/raw_df.rds")
cat("  Current columns:", ncol(raw_df), "\n")

gene_start_idx_raw <- min(which(grepl("^PVP01", names(raw_df))))
meta_end_idx_raw <- gene_start_idx_raw - 1
cat("  Metadata ends at column:", meta_end_idx_raw, "\n")

raw_df_meta <- raw_df[, 1:meta_end_idx_raw]
raw_df_expr <- raw_df[, (meta_end_idx_raw + 1):ncol(raw_df)]
raw_df_meta <- raw_df_meta[, !names(raw_df_meta) %in% cols_to_remove]
raw_df <- cbind(raw_df_meta, new_cols, raw_df_expr)
cat("  Updated columns:", ncol(raw_df), "\n")
saveRDS(raw_df, "data/raw_df.rds")

cat("\n✓ DONE!\n")
cat("\nFinal column names:\n")
cat("  - development_phase (was development_stage)\n")
cat("  - blood_stage (was blood_stage_form)\n")
cat("  - parasite_stage (was parasite_stage_overall)\n")
cat("\nAll files updated.\n")
