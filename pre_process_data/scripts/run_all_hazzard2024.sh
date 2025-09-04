#!/usr/bin/bash

# List of R scripts
scripts=(
  "hazzard2024_pv_combined_55_56.R"
  "hazzard2024_pv_combined_59_65.R"
  "hazzard2024_pv_combined_66_72.R"
  "hazzard2024_pv_combined_75_76.R"
  "hazzard2024_pv_combined_80_87.R"
  "hazzard2024_pv_combined_91_94.R"
)

# Loop through and run each one
for script in "${scripts[@]}"; do
  echo "Running $script"
  Rscript "$script"
  if [ $? -ne 0 ]; then
    echo "Error in $script, continuing to next ${script}..."
  fi
done

echo -e "\nrunning hazzard2024_merge_all..."

Rscript hazzard2024_merge_all.R

echo -e "\nrunning hazzard 2022..."
Rscript hazzard2022_pv_analysis_script.R

echo -e "\nrunning ruberto2022 1..."
Rscript ruberto2022_1_pv_analysis_script.R

echo -e "\nrunning ruberto2022 2..."
Rscript ruberto2022_2_pv_analysis_script.R

echo -e "\nrunning sa2020..."
Rscript sa2020_pv_analysis_script.R

echo -e "\nrunning integration..."
Rscript integration.R

echo "All scripts completed."






