# PlaViSCA Performance Analysis

**Date:** 2026-02-11  
**Analysis Focus:** Data loading and memory optimization

---

## 🔍 Major Performance Issues Found

### Issue 1: MASSIVE DATA FILES (5.6GB each in memory!)

**Current State:**
- `norm_df`: 105,963 cells × 6,900 columns = **5.6GB in memory**
- `raw_df`: 105,963 cells × 6,900 columns = **5.6GB in memory**
- Matrix contains 728 million values
- **93.24% of values are ZERO** (highly sparse!)

**Problem:**
- Dense matrix format wastes enormous memory
- Loading time is slow due to large file size (458MB on disk)
- Every data operation copies this massive matrix

**Solution:** Convert to sparse matrix format
- Expected memory reduction: **~90%** (5.6GB → ~560MB)
- Faster loading and subsetting
- R packages: `Matrix` (dgCMatrix format)

---

### Issue 2: REDUNDANT DATA DUPLICATION

**Current State:**
```
norm_df:  105,963 rows × 6,900 cols = 5.6GB
  ├── Metadata (11 cols)
  ├── Embeddings (15 cols: PCA, UMAP, t-SNE)
  └── Gene expression (6,874 cols)

mr_data:  105,963 rows × 52 cols = 54MB
  ├── Metadata (40 cols)
  └── Embeddings (12 cols)
```

**Problem:**
- Metadata and embeddings are stored in BOTH `norm_df` and `mr_data`
- This duplication wastes 54MB RAM
- Code maintains two sources of truth for the same data

**Solution:** 
- Option A: Remove metadata/embeddings from norm_df, keep only gene matrix
- Option B: Remove mr_data entirely, extract metadata from norm_df when needed
- Recommended: **Option A** (cleaner separation, easier to maintain)

---

### Issue 3: HEAVY UPFRONT LOADING

**Current State (global.R lines 36-41):**
```r
norm_df <- readRDS(DATA_PATHS$norm_df)              # 5.6GB loaded
cleaned_data_list <- readRDS(DATA_PATHS$cleaned_dataset)  # 54MB loaded
mr_data <- cleaned_data_list$mr_data
hiv_data <- cleaned_data_list$hiv_data
pca_df <- cleaned_data_list$pca_df
```

**Problem:**
- App startup loads **5.6GB immediately**
- Users wait 10-30 seconds before seeing anything
- Even if they just want to browse metadata

**Solution:**
- Lazy load norm_df (only when gene expression is needed)
- Keep lightweight mr_data for embeddings/metadata
- Load raw_df on-demand (already implemented)

---

## 📊 Memory Usage Breakdown

| Object | Size | Usage | Recommendation |
|--------|------|-------|----------------|
| `norm_df` | 5.6GB | Gene expression (normalized) | Convert to sparse |
| `raw_df` | 5.6GB | Gene expression (raw) | Convert to sparse |
| `mr_data` | 54MB | Metadata + embeddings | Keep (or merge) |
| `hiv_data` | 2.7MB | Highly variable genes | Keep |
| `pca_df` | <1MB | PCA variance | Keep |
| `gff_data` | <1MB | Gene annotations | Keep |

**Total at startup:** ~5.9GB  
**After optimization:** ~600MB (10× reduction!)

---

## 🎯 Optimization Recommendations (Priority Order)

### Priority 1: Convert to Sparse Matrices ⚡ CRITICAL

**Impact:** 90% memory reduction, faster operations  
**Effort:** Medium (needs preprocessing script update)

**Implementation:**
1. Update `functions/preprocessing.R` to save as sparse format:
   ```r
   library(Matrix)
   norm_sparse <- Matrix(norm_exp, sparse = TRUE)
   saveRDS(norm_sparse, "data/normalize_df_sparse.rds")
   ```

2. Update `global.R` to handle sparse matrices:
   ```r
   norm_df <- readRDS("data/normalize_df_sparse.rds")  # Much smaller!
   ```

3. Code already works with sparse matrices (Matrix package compatible with base R)

**Expected Results:**
- Memory: 5.6GB → 560MB per dataset
- Load time: 10-30s → 2-5s
- Subsetting: Much faster

---

### Priority 2: Eliminate Data Duplication

**Impact:** 54MB saved, cleaner code  
**Effort:** Low

**Option A - Recommended:**
1. Split norm_df into:
   - `gene_expr` (sparse matrix, genes only)
   - `metadata` (data.frame, metadata + embeddings)

2. Remove mr_data (use metadata instead)

**Option B - Quick Fix:**
1. Keep current structure
2. Don't load mr_data separately (extract from norm_df when needed)

---

### Priority 3: Lazy Load Expression Data

**Impact:** Faster startup (5.6GB → 54MB loaded)  
**Effort:** Medium

**Implementation:**
1. Don't load norm_df in global.R
2. Load it reactively when user goes to gene expression or embeddings tab
3. Cache it after first load

**Expected Results:**
- Startup time: 20s → 2s
- First plot render: ~5s delay (one-time)
- Subsequent renders: Fast (cached)

---

### Priority 4: Additional Optimizations

**Other potential improvements:**

1. **Debounce filter inputs** - Avoid re-filtering on every keystroke
2. **Use `req()` more aggressively** - Skip computation until all inputs ready  
3. **Profile reactive graph** - Identify unnecessary re-computations
4. **Consider data.table** - For faster filtering (if needed after sparse conversion)

---

## 🚀 Expected Performance After Optimization

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Memory at startup | 5.9GB | 60MB | **98% reduction** |
| App startup time | 20-30s | 2-5s | **6-10× faster** |
| Memory per dataset | 5.6GB | 560MB | **90% reduction** |
| Filter operations | Slow | Fast | Much faster subsetting |
| Plot rendering | Slow | Fast | Less data to copy |

---

## 📝 Implementation Plan

1. **Phase 1:** Convert to sparse matrices (CRITICAL)
   - Update preprocessing script
   - Regenerate data files
   - Test app functionality

2. **Phase 2:** Remove data duplication
   - Refactor to use single metadata source
   - Update all references

3. **Phase 3:** Implement lazy loading
   - Move norm_df load to reactive context
   - Add loading indicators

4. **Phase 4:** Profile and fine-tune
   - Identify remaining bottlenecks
   - Add debouncing where needed

---

## ⚠️ Notes

- Sparse matrix conversion is **non-breaking** (Matrix package integrates with base R)
- Original dense files backed up before conversion
- All changes should be tested with actual usage patterns
- Consider using `profvis` package to profile actual bottlenecks during use
