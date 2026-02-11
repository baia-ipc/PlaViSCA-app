# Parasite Stage Terminology Update

**Date:** 2026-02-11  
**Purpose:** Standardize terminology for consistency and clarity

---

## Current vs. New Terminology

### Current Structure

**Two variables in data:**
1. `life_cycle_stage` (9 unique values):
   - Female gametocyte
   - Hypnozoite
   - Male gametocyte
   - Merozoite
   - Ring
   - Schizont (blood stage)
   - Schizont (liver stage)
   - Sporozoite
   - Trophozoite

2. `parasite_stage` (3 unique values):
   - Blood stage
   - Liver stage
   - Sporozoite stage

---

## New Terminology Framework

### 1. Development Phases (High-level grouping)
- **Sporozoite** (replaces "Sporozoite stage")
- **Liver stages** (replaces "Liver stage")
- **Blood stage** (remains the same)

### 2. Blood-stage Forms (Subset of Blood stage)
- Merozoites
- Ring
- Trophozoite
- Schizont (blood stage)
- Female gametocyte
- Male gametocyte

### 3. Parasite Stages (All specific forms)
- Sporozoite
- Merozoites
- Ring
- Trophozoite
- Schizont (blood stage)
- Female gametocyte
- Male gametocyte
- Hypnozoite (liver stage specific form)
- Schizont (liver stage)

---

## Implementation Plan

### Option A: Rename Existing Variables (Recommended)
Keep data structure but update labels in UI:

**Variable: `parasite_stage`** → Rename to **"Development phase"** in UI
- Sporozoite stage → Sporozoite
- Liver stage → Liver stages
- Blood stage → Blood stage

**Variable: `life_cycle_stage`** → Rename to **"Parasite stage"** in UI
- Keep all current values (already correct)
- These are the detailed forms

### Option B: Create New Grouping Variable
Add a new variable to distinguish blood-stage forms:

**New variable: `blood_stage_form`**
- Only populated for cells where `parasite_stage == "Blood stage"`
- Values: Merozoites, Ring, Trophozoite, Schizont (blood stage), Female gametocyte, Male gametocyte

---

## Recommended Approach: Option A

### UI Changes Needed

1. **Column Headers & Labels:**
   - "Life cycle stage" → "Parasite stage"
   - "Parasite stage" → "Development phase"

2. **Filter Labels:**
   - Update filter labels in metadata filters
   - Update group-by options in plots

3. **Value Labels (parasite_stage):**
   - "Sporozoite stage" → "Sporozoite"
   - "Liver stage" → "Liver stages"
   - "Blood stage" → "Blood stage" (no change)

4. **Hover Text & Tooltips:**
   - Update all references to use new terminology

### Files to Update

1. `ui.R` - Update all labels and choices
2. `server.R` - Update dynamic filter generation
3. `global.R` - Update column definitions
4. `functions/helpers_UI.R` - Update helper functions
5. `functions/helpers_server.R` - Update export/download labels
6. `functions/embedding_plots.R` - Update hover text
7. `www/style.css` - Update any hardcoded labels (if any)

---

## Data Integrity

**No data changes required** - this is purely a labeling/presentation update.

The underlying data variables remain:
- `life_cycle_stage` (specific forms) → Display as "Parasite stage"
- `parasite_stage` (broad phases) → Display as "Development phase"

---

## Notes

- This maintains backward compatibility with existing data
- No preprocessing required
- Can be reversed easily if needed
- Users will see clearer, more scientifically accurate terminology
