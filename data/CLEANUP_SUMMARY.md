# Data Cleanup Summary

**Date:** 2026-02-11
**Branch:** dual-embedding-plots

## Overview
Cleaned up unused and redundant data files to reduce storage footprint while maintaining full application functionality.

## Files Moved to Backup

Location: `data/backup_unused_20260211/`

| File | Size | Reason |
|------|------|--------|
| normalize_df.rds | 102M | Replaced by normalize_df_real.rds (458M) |
| scale_df.rds | 204M | Scaled data not used in app |
| Pv.all.12.Jan2025.rds | 122M | No code references found |
| all_features.rds | 21M | Never used in codebase |
| gff_data.rds | 69K | Duplicate (app uses ref/PvivaxP01_gff_data.rds) |
| **TOTAL** | **449M** | |

## Active Data Files (Kept)

| File | Size | Purpose |
|------|------|---------|
| normalize_df_real.rds | 458M | Main normalized expression matrix |
| raw_df.rds | 102M | Raw count data (lazy loaded) |
| cleaned_dataset.rds | 13M | Metadata, embeddings, QC data |
| data_source.csv | 1.6K | Data source information |
| **TOTAL** | **573M** | |

## Storage Impact

- **Before cleanup:** 1.2GB (active + backup + original)
- **Active data after cleanup:** 573M
- **Files backed up:** 449M
- **Original files (kept):** 135M
- **Reduction in active data:** 44% (1022M → 573M)

## Testing Results

All tests passed ✓

1. ✓ global.R loads successfully
2. ✓ All required data objects exist (norm_df, mr_data, hiv_data, pca_df, gff_data)
3. ✓ DATA_PATHS configured correctly
4. ✓ All data file paths valid
5. ✓ ui.R loads successfully
6. ✓ server.R parses correctly

## Recovery Instructions

If you need to restore any backed-up file:

```bash
# Restore a specific file
cp data/backup_unused_20260211/filename.rds data/

# Restore all files
cp data/backup_unused_20260211/*.rds data/
```

## Permanent Deletion

The backup directory can be safely deleted after confirming the app runs correctly in production for 1-2 weeks.

```bash
# After verification period
rm -rf data/backup_unused_20260211/
```

## Notes

- All original source files remain in `data/original_file/` (135M)
- The preprocessing script `functions/preprocessing.R` can regenerate scale_df.rds and normalize_df.rds if needed
- No code changes were required - only data file organization
