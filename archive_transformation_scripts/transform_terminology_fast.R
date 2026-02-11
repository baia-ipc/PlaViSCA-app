# Quick transformation - only update cleaned_dataset (small file)
# The large expression matrices will inherit columns from mr_data

library(dplyr)

cat("=== TRANSFORMING PARASITE STAGE TERMINOLOGY ===\n\n")

# Backup first
backup_dir <- "data/backup_old_terminology_20260211"
dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)

cat("Creating backup...\n")
file.copy("data/cleaned_dataset.rds", 
          file.path(backup_dir, "cleaned_dataset.rds"),
          overwrite = TRUE)

# Load only the small file
cat("Loading cleaned_dataset.rds...\n")
cleaned_data <- readRDS("data/cleaned_dataset.rds")

cat("Current mr_data columns:", ncol(cleaned_data$mr_data), "\n")
cat("Current mr_data rows:", nrow(cleaned_data$mr_data), "\n\n")

# Transform mr_data
cat("Adding new terminology columns...\n")
cleaned_data$mr_data <- cleaned_data$mr_data %>%
    mutate(
        # 1. Development phase
        development_phase = case_when(
            parasite_stage == "Sporozoite stage" ~ "Sporozoite",
            parasite_stage == "Liver stage" ~ "Liver stages",
            parasite_stage == "Blood stage" ~ "Blood stage",
            TRUE ~ parasite_stage
        ),
        
        # 2. Blood-stage form
        blood_stage_form = case_when(
            life_cycle_stage == "Merozoite" ~ "Merozoites",
            life_cycle_stage == "Ring" ~ "Ring",
            life_cycle_stage == "Trophozoite" ~ "Trophozoite",
            life_cycle_stage == "Schizont (blood stage)" ~ "Schizont (blood stage)",
            life_cycle_stage == "Female gametocyte" ~ "Female gametocyte",
            life_cycle_stage == "Male gametocyte" ~ "Male gametocyte",
            TRUE ~ NA_character_
        ),
        
        # 3. Parasite stage overall
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

cat("New mr_data columns:", ncol(cleaned_data$mr_data), "\n\n")

# Verification
cat("=== VERIFICATION ===\n\n")
cat("Development phase:\n")
print(table(cleaned_data$mr_data$development_phase))
cat("\n\nBlood-stage form:\n")
print(table(cleaned_data$mr_data$blood_stage_form, useNA = "ifany"))
cat("\n\nParasite stage overall:\n")
print(table(cleaned_data$mr_data$parasite_stage_overall))

# Save
cat("\n\nSaving...\n")
saveRDS(cleaned_data, "data/cleaned_dataset.rds")

cat("✓ DONE!\n")
cat("Note: Large files (norm_df_real.rds, raw_df.rds) will get these columns from mr_data at runtime\n")
