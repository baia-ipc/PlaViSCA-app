# Backup of Unused Data Files

**Date:** $(date +"%Y-%m-%d %H:%M:%S")
**Reason:** Cleanup of redundant/unused data files to reduce storage

## Files Moved to Backup

### 1. normalize_df.rds (102M)
- **Reason:** Replaced by normalize_df_real.rds (458M)
- **Status:** Commented out in global.R (line 30)
- **Safe to delete:** Yes, not referenced in active code

### 2. scale_df.rds (204M)
- **Reason:** Scaled expression data not used in application
- **Status:** Commented out in global.R (line 30)
- **Safe to delete:** Yes, can be regenerated from preprocessing.R if needed

### 3. Pv.all.12.Jan2025.rds (122M)
- **Reason:** Not referenced in any R code, possible duplicate
- **Status:** No code references found
- **Safe to delete:** Likely yes, but verify it's not needed as source data

### 4. all_features.rds (21M)
- **Reason:** No references found in codebase
- **Status:** Never loaded or used
- **Safe to delete:** Yes

### 5. gff_data.rds (69K)
- **Reason:** Duplicate - app uses ref/PvivaxP01_gff_data.rds instead
- **Status:** Different file loaded in global.R
- **Safe to delete:** Yes

## Total Space Saved
- **Before cleanup:** ~1.2GB
- **Files backed up:** ~449MB
- **After cleanup:** ~751MB
- **Savings:** 37% reduction

## Recovery Instructions
If you need any of these files back:
```bash
cp data/backup_unused_YYYYMMDD/filename.rds data/
```

## Verification
After cleanup, the app was tested and confirmed working with:
- Normalized data: normalize_df_real.rds
- Raw data: raw_df.rds  
- Metadata: cleaned_dataset.rds
- GFF data: ref/PvivaxP01_gff_data.rds

## Permanent Deletion
These files can be permanently deleted after confirming the app works correctly for 1-2 weeks.
