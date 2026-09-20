# PlaViSca Phase-3 App Repair — Audit Report

**Status: FINAL APP RELEASE CANDIDATE** (updated 2026-09-21 — see section 26)
(not merged to `master`, not deployed — see *Handover* at the end)

| Item | Value |
|---|---|
| App repo | `PlaViSca` |
| App base commit | `020f9cbd651750ba92c037399fb84d3b17d389e8` (`master`, "simplify README more") |
| App branch | `plavisca-app-production-repair` |
| Preprocessing repo commit consumed (first pass) | `54c6b9581ff5268422ddffda42ce457bf09c5b55` ("Rebuild PlaViSca atlas with validated Ruberto counts", branch `plavisca-production-repair`) |
| Preprocessing closure SHA (final, authoritative) | `dea656faa2b06381cc971b462de8f911df1028f7` (branch `plavisca-release-closure`) — see section 26 |
| Candidate export path used for validation | `pre_process_data/data/candidate_export/` (+ `pre_process_data/data/data_source.csv`), reached through a symlink staging directory outside both repos |
| Atlas size validated | 105,963 cells x 88 metadata fields x 6,860 genes, 6 studies |
| Tests (final candidate) | 68 run / **67 passed** / 0 failed / 1 skipped (environment-only, see section 26) |
| Startup contract checks (final candidate) | 33 run / **33 PASS** / 0 WARN / 0 FAIL |
| AT19 | **PASS** |
| AT20 | **PASS** |
| Date | 2026-09-20 |

---

## 1. Candidate artefacts actually tested

See `candidate_data_manifest.tsv` for the full table.

| File | SHA256 | In `build_manifest.tsv`? |
|---|---|---|
| `cleaned_dataset.rds` | `30b61ccdad19ef52eb835bf4e644800b2151497d14066438bd926a717d3f37a0` | verified — matches |
| `normalize_df.rds` | `8a87841094655148422d157635e6ed50d4bb9dba7735798cc9e415ae069a8740` | not recorded upstream |
| `raw_df.rds` | `15df4435d666cef1e3ec4d2d977f507a4415ecf3a8bd5d952199dda42e009cf8` | not recorded upstream |
| `scale_df.rds` | `86d08ff3eea411c01437bc0f6ca461bd65184ebba032cab381adb700e7ea1cbf` | not recorded upstream (not loaded by the app) |
| `data_source.csv` | `cb1dca3ff9247634eda474dde693ce16382343cf5bd29e70a59c765c27bdaf98` | verified — matches |

`cleaned_dataset.rds` and `data_source.csv` hashes were verified byte-identical to the values recorded in `pre_process_data/build_manifest.tsv`. The three wide expression tables are not listed in that manifest; their hashes are recorded here so the exact artefacts tested are reproducible. **This is a non-blocking gap for the project lead to note:** the app was validated against the exact files above, but there is no upstream manifest entry to cross-check `normalize_df.rds` / `raw_df.rds` against.

No candidate file was modified. No file under `PlaViSca/data/` (the existing deployed data) was modified. No write or git operation was performed in `pre_process_data`.

## 2. Candidate-data switching mechanism

`PLAVISCA_DATA_DIR` (default `data`) is now the single origin of every application data path. `functions/data_contract.R` exposes `app_data_dir()` / `app_data_path()` / `app_data_paths()`, and `global.R` builds `DATA_PATHS` from them. The bibliography CSV, which `server.R` previously re-read from the literal string `"data/data_source.csv"`, now also resolves through this mechanism and is loaded (and validated) once at startup.

An AT20 static test asserts that no `"data/..."` literal and no absolute `/home|/srv|/Users` path survives anywhere in the live application code.

Testing used a staging directory of symlinks **outside the repository**
(`/home/baia/prj/plavisca/phase3_candidate_staging`), so no large binary and no machine-specific symlink is committed. `data/*` remains gitignored as before.

**Loud failure on incompatible data was verified empirically.** Running `global.R` with `PLAVISCA_DATA_DIR=data` (the old pre-Phase-2 production export) now aborts with:

```
PlaViSca startup data-contract validation FAILED (2 check(s)).
The data in 'data' is not compatible with this version of the app.
  - configured_fields_resolve: required field(s) map to columns missing from the data:
      broad_stage -> parasite_broad_stage, detailed_stage -> harmonized_life_cycle_stage, ...
  - schema_compatibility: the loaded dataset does not carry the Phase-2 atlas schema this
      version of the app requires; remaining checks were skipped.
```

## 3. Metadata / configuration migration

`config/data_columns.yml` was rewritten from 13 loosely-specified fields to a complete, documented configuration layer that is now the app's **single source of truth**. It declares 40 fields (all required and all present in the candidate), plus the filter set, the category ("Color by"/"Group by") set, the detail-table field order, biological vocabularies, the detailed-to-broad stage mapping, explicit colours, human-readable value labels, italicised fields and UI note text.

`functions/data_columns_config.R` grew from 5 helpers to a full accessor API: label/help/column lookup, role-based field queries, biological ordering (`order_field_values`, `order_column_values`), colour resolution, display-value mapping, missing-value relabelling, stage-compatibility queries, and a strict validator.

Duplicate vocabularies that bypassed the config layer were removed:

* `server.R` lines ~1786 and ~2262 — two hardcoded label maps keyed by physical column name (D064). Both now call `get_label_for_column()`.
* `functions/helpers_server.R` — `FILTER_FIELDS` is now `filter_field_keys()`; `apply_secondary_filters()`'s hardcoded 14-column list is now `app_metadata_columns()`; `generate_data_summary()`'s hardcoded list is config-resolved; the hardcoded species allow-list for italics was replaced by a binomial-pattern test driven by `italicised_fields`.
* `filtered_gene_exp_cache_key()`, the metadata/QC observer trigger lists, the cross-tab filter-sync list, the two Excel export metadata blocks and the filter-dropdown population loop are all now generated from `filter_field_keys()`.

**Field mapping (verified against the candidate — see `field_mapping.tsv`).** Notable resolutions where the task spec's paraphrased names differed from reality:

| Concept | Spec name | Actual column used | Why |
|---|---|---|---|
| Author sex annotation | `source_sex_annotation` | `source_sex_annotation` | present as named — the spec's caveat was unnecessary |
| Author grouping | `source_pseudogroup` | `source_pseudogroup` | present as named |
| Technology | "technology" | **`sc_technology`** | the column literally named `technology` is populated only for Hazzard2024 (80,024/105,963 NA); `sc_technology` is populated for every cell and carries the corrected vocabulary. Documented decision. |
| Sequencer | "sequencer field" | **`sequencer_registered`** | the ENA/GEO-registered instrument per `metadata_schema.tsv` DEC05; `sequencer_protocol_publication` is retained as a separate detail field, never silently reconciled |
| Author stage label | `source_stage_annotation` | **`source_life_cycle_stage`** | no column named `source_stage_annotation` exists in the export; `source_life_cycle_stage` is the verbatim author label. The study-specific verbatim fields (`source_state`, `source_ap2g`, `source_sex_state`, `source_orig_ident`) exist but are Mancio-Silva-only and were left out of the shared UI. |
| Strain | `strain` (old filter) | **`parasite_lineage_isolate`** | `strain` is NA for all 80,024 Hazzard2024 cells; `parasite_lineage_isolate` is the Phase-2 corrected field (fixes D027/D042). The `strain` filter was replaced. |

`adjudicated_display_stage` is **deliberately not exposed**: it is entirely NA by design (DEC07 is unresolved) and a test asserts the app neither configures it nor finds it populated.

## 4. Broad / detailed stage naming trap — fixed

The inverted config keys are gone:

| Old logical key | Old physical column | New logical key | New physical column |
|---|---|---|---|
| `development_phase` (broad) | `parasite_stage` | **`broad_stage`** | `parasite_broad_stage` |
| `parasite_stages` (detailed) | `life_cycle_stage` | **`detailed_stage`** | `harmonized_life_cycle_stage` |

Shiny input IDs were renamed too (`broad_stage*`, `detailed_stage*`), so no ambiguous identifier survives in the UI. The smoke test asserts the rendered HTML contains `broad_stage`/`detailed_stage` and contains **neither** the old broad key nor the old HPI key.

## 5. Broad/detailed filtering is now logically safe

The deterministic mapping table from `metadata_schema.tsv` (the `parasite_broad_stage` row) is transcribed into `config/data_columns.yml` as `stage_compatibility:` and is **validated against the data at startup** — if the export ever disagreed with the config, the app would refuse to start. `stage_compatibility.tsv` records the per-stage verification: all 7 detailed stages present in the candidate map exactly as documented; the 4 vocabulary stages with no rows today (EEF, Male/Female gametocyte, Asexual (unresolved stage)) are kept in the vocabulary but never invented into any UI list.

Behaviour:

* Selecting a broad stage constrains the detailed-stage dropdown to compatible values, and drops now-incompatible selections.
* An impossible combination arriving anyway (old bookmark, restored session) shows an explicit warning banner under the filter row explaining *why* zero rows would match and which filter to clear — instead of silently rendering an empty plot.
* `stage_selection_is_possible()` / `stage_selection_message()` are pure functions, directly unit-tested.

No biology was invented: the mapping is a verbatim transcription of the upstream authority.

## 6. IDC / HPI semantics — fixed

The obsolete hour-post-invasion field is gone from the app entirely (config, input IDs, filters, plots, exports, cache keys). The app-visible field is `idc_reference_similarity_label`, labelled **"IDC reference-time class"**, with the tooltip/footer text:

> SingleR expression-similarity classification against reference intra-erythrocytic development cycle (IDC) timepoints; this is not a measured hour-post-invasion value. Only blood-stage-eligible cells carry a value; all other cells are Missing by design.

Tests assert the label is not "HPI" and does not contain "hour post", and that the help text explicitly disclaims measured-time semantics.

The three uncertainty companions (`idc_similarity_raw_label`, `idc_similarity_score_delta`, `idc_similarity_pruned_flag`) are configured as detail fields. `idc_heuristic_stage_bin` is exposed under the label **"PlaViSca heuristic IDC stage"** with a footer stating the cutoffs are PlaViSca-defined and were not established by the reference publication.

No values are manufactured for NA cells: 15,744 cells legitimately carry a true NA and are displayed as "Missing". The startup validator confirms 0 non-blood cells carry an IDC class.

## 7. Gametocyte provenance — not collapsed

`pred_gametocyte_sex` ("Gametocyte sex (PlaViSca-inferred)") and `source_sex_annotation` ("Gametocyte sex (author-reported)") are two separate, separately-labelled fields with their own metadata cards and help text. `source_pseudogroup` ("Author pseudogroup") is exposed alongside them, and `source_stage_provenance` ("Stage annotation provenance") tells the user which mechanism produced a given cell's harmonized stage.

The asymmetric hardcoded colour override (Female gametocyte only) is gone. Both sexes now have explicit, distinct configured colours, applied through one deterministic helper (`category_palette()`), and a test asserts the symmetry.

A test also asserts the two fields genuinely disagree for some cells — i.e. verifies they were not quietly merged upstream. (In the candidate: 1,323 agree female, 86 agree male, 20 disagree, plus 103 author-female cells with no inferred call.)

## 8. Treatment

The 4-state vocabulary is preserved exactly. `study_control` and `No_Treatment` remain distinct categories throughout; they are only given human-readable *display* labels ("Study control", "No treatment") via the config `value_labels:` map, which never touches the underlying data. The treatment card carries the schema's rationale as footer text. Defensive literal-"NA"/"None" handling is implemented generically in `relabel_missing()` / `is_missing_value()` and asserted absent from every app-visible categorical field (the only literal "NA" strings left in the export are in `source_treatment`, which is internal provenance and not app-visible).

## 9. Geography

`parasite_origin_location` is the primary app-visible geography field (filterable, plotted, in the detail table) and replaces the conflated legacy geography column, which no longer appears anywhere. `host_provenance`, `experimental_site`, `sequencing_site` and `registry_location` are retained as four *separate* detail fields; `registry_location`'s help text explicitly says it is a submission artefact and not biological provenance. A test asserts all five remain distinct columns.

## 10. Biological replicate visibility

`animal_id`, `donor_id`, `biological_replicate_type` are detail-view fields, and `biological_replicate_id` is additionally available as a configured filter field. `library_id` is exposed as a detail field. A test asserts all four are present and surfaced.

## 11. Bibliography / data-source validation (closes D056/D057/D067)

The rebuilt Phase-2 bibliography is `pre_process_data/data/data_source.csv` (regenerated by `scripts/data_source_manipulation.R`, which now sources study labels from the shared `STUDY_LABELS` constant). It is loaded once in `global.R` and **validated against the atlas at startup** by `validate_bibliography()`:

* exactly 6 records, no duplicates — **PASS**
* every atlas study has a record — **PASS**
* no bibliography record without atlas cells — **PASS**
* per-study cell counts agree (9766 / 9947 / 1438 / 3294 / 80024 / 1494) — **PASS**
* each study carries exactly one PMID in the atlas — **PASS**
* PMID cross-check between the two sources — **WARN (non-blocking)**

**Documented limitation:** the rebuilt `data_source.csv` has no PMID column (its columns are Publication_date, Study_label, Authors, Journal, Title, DOI, Number_of_cells), so the only available join key is the canonical `study_label`. The validator reports this explicitly as a WARN rather than silently claiming a PMID check it cannot perform. *Alternative rejected:* deriving PMIDs by parsing the DOI HTML strings — that would have invented a join key not present in the authoritative export. **Recommendation for the project lead:** add `study_pmid` to `data_source_manipulation.R`'s output so the D067 key can be checked end-to-end. See `bibliography_validation.tsv` and `bibliography_study_join.tsv`.

Tests also verify the validator *detects* a deliberately removed record and a deliberately added extra record — i.e. that it is not vacuous.

## 12. Stale / dead code removed

Nine files were moved to `docs/legacy/` with a mandatory header marking them historical, not-sourced, and possibly referencing columns that no longer exist. `global.R` no longer sources any of them.

| File | Why it was dead |
|---|---|
| `preprocessing.R` | stale offline generator; referenced a column literally named with the old plural stage name. The only authoritative pipeline is `pre_process_data`. |
| `seurat_preprocessing.R` | same category; never sourced |
| `UTF-8Pv_singleR_script.Rmd` | historical SingleR notebook, superseded by `pre_process_data/scripts/singleR.R` |
| `metadata_filter_module.R` | **all 5 functions unreferenced**; directly accessed four metadata columns that do not exist in any shipped schema (D063) |
| `quality_control_module.R` | unreferenced duplicate of server.R's QC block; referenced `*_qc` inputs under the old names |
| `gene_expression_module.R` | unreferenced duplicate; same problem |
| `data_loading_module.R` | unreferenced; `load_dataset`/`get_current_data_type` are defined locally inside `server()` and shadow it |
| `embedding_visualization_module.R` | unreferenced; `generate_embedding_plot` is defined locally inside `server()` |
| `navigation_module.R` | unreferenced duplicate of server.R's navigation observers |

Verification: every function defined in these files was grepped against `server.R`/`global.R`/`ui.R` and found to have **zero** references before removal. The parse sweep and the smoke test confirm the app still constructs and runs.

Also removed from live code:

* `server.R` L626-667 — the dead `grouped_choices` list (D063), never referenced by any UI element.
* `functions/embedding_plots.R` — the unreachable dead branch in `prepare_categorical_colors()` (D065) that tested for a column name which has never existed.

## 13. Silent error masking removed

* `filtered_gene_data` previously wrapped `apply_secondary_filters()` in a `tryCatch` that returned **the unfiltered atlas** on error — i.e. a schema failure would silently show all 105,963 cells as if the user's filters had been applied. Removed.
* `meta_plotly()`'s missing-column `stop()` now identifies itself as a data-contract error and names the config file to check, instead of a bare message.
* `plot_2d`/`plot_3d`'s invalid-colour fallback no longer silently switches to a hardcoded column name; it falls back to the *configured* broad-stage column, warns, and hard-errors if even that is absent.
* The filter-dropdown population loop no longer swallows all errors. A configured column missing from the data now raises a persistent error notification naming the offending field **and** states that the data is not filtered by it.
* `validate_data_columns()` was changed from `warning()` to `stop()` for required fields.

User-input emptiness is still treated as "no constraint" and never errors — the distinction between user emptiness and programmer/data-contract failure is explicit.

## 14. Expression / export integrity

The atlas was **not** recomputed. Verified against the candidate:

* `normalize_df` and `raw_df` are 105,963 x 6,948 with identical row names, identical column names, no duplicate cell ids, and the same cell set as `mr_data`.
* The 88 metadata columns form a contiguous prefix; the 6,860 gene columns all match `^PVP01_`.
* **Gene identification is by identifier, never by position.** `identify_gene_columns()` selects `^PVP01_` columns and subtracts the metadata column names. A test explicitly asserts that column 53 — which *was* the first gene column under the old ~52-field schema — is still metadata today, so any surviving positional assumption would be caught.
* `genes()` in `helpers_server.R` already matched by pattern, and produces exactly 6,860 rows.
* Row-index alignment verified end to end: filtering to "Blood stage + Ring" returns 8,521 cells, and `build_plot_slim()` returns 8,521 aligned rows with correct per-cell expression values.
* `hiv_data` (HVG), `top_genes_exp` and `pca_df` each cover all six studies.

**Note for the record:** `mr_data`'s row order differs from `normalize_df`'s row order (the metadata starts with Hazzard2022, the expression tables with Sa2020). This is *pre-existing and harmless* because every code path that uses `row_idx` derives it from `gene_data()$data` (i.e. from `normalize_df`/`raw_df`, which carry their own copy of all 88 metadata columns) and indexes back into the same table. The contract validator asserts set-equality of cell keys rather than order-equality, and a test asserts the barcode column stays aligned with the row names after subsetting.

## 15. Feature-availability caveat (v1 policy)

No feature-availability UI was built, as instructed. A concise info banner was added to the Gene expression tab, sourced from the config `notes:` block:

> Feature coverage differs between studies. Ruberto2022_1 was quantified against an older PlasmoDB-51 reference, so 2,089 of PlaViSca's features are not available from that study's author matrix and are recorded as zero. A displayed zero therefore does not necessarily mean the gene was assayed and not detected in every study.

## 16. Missing-value display

One generic mechanism: `MISSING_VALUE_LABEL` (configured as `"Missing"`), `relabel_missing()` and `is_missing_value()`. These are display-only — a test asserts the NA count of the underlying data is unchanged after relabelling. `LITERAL_MISSING_TOKENS` provides defence-in-depth against residual "NA"/"None"/"" strings. Missing always sorts last in category orderings and always gets the same neutral grey.

## 17. Biological category ordering

Centralised in `config/data_columns.yml` under `vocabularies:` and applied through `order_field_values()` / `order_column_values()` — no ordering logic in plot code.

* **Detailed stage:** Sporozoite -> EEF -> Hypnozoite -> Schizont (Liver stage) -> Ring -> Trophozoite -> Schizont (Blood stage) -> Merozoite -> Male gametocyte -> Female gametocyte -> Asexual (unresolved stage)
* **Broad stage:** Sporozoite stage -> Liver stage -> Blood stage -> Mosquito stage (retained in the vocabulary per schema although it has no rows today)

Levels absent from the loaded data are dropped, never invented — a test asserts the rendered ordering is exactly set-equal to the levels actually present. Levels present but *not* in the vocabulary are appended and remain visible rather than silently disappearing.

## 18. Performance

No performance rewrite was attempted. The existing `memoise`d loader, the `cached_data` reactiveValues store and the `sessionData` cache are unchanged. One improvement: the bibliography CSV is now read once at startup rather than inside a reactive. One neutral change: `apply_secondary_filters()`'s metadata column list grew from 14 to 40 config-resolved names; the branch logic it feeds behaves identically on this data (the non-metadata column count remains far above the threshold). Startup loads `normalize_df` exactly once; `raw_df` is still loaded lazily on first use. **No memory regression observed.**

## 19. Testing

`tests/` (new) contains a dependency-free harness plus six test files and a runner:

```
PLAVISCA_DATA_DIR=<candidate dir> Rscript tests/run_tests.R [out.tsv]
```

`testthat` is not installed in this environment (and is not in the usable runtime library), so plain R scripts were used, as the spec permits. Exit status is non-zero on failure. **Result: 68 tests, 68 passed, 0 failed, 0 skipped.** Full listing in `test_results.tsv`.

Coverage: data contract (11), lifecycle (13), IDC + gametocytes + treatment + geography + replicates (16), bibliography + filters (12), expression (6), AT19 + AT20 (9).

## 20. Parse / source / smoke test

* **Parse sweep:** all live R files (`app.R`, `global.R`, `ui.R`, `server.R`, `functions/*.R`) parse cleanly.
* **Startup source test:** `global.R`, `ui.R` and `server.R` all source successfully in a fresh non-interactive R process against the candidate data.
* **Smoke test:** UI object constructs (147,334 chars of HTML) and contains the renamed input IDs and the corrected IDC label while containing no obsolete IDs; `server` constructs with `(input, output, session)`; startup validation passes; all 8 filter fields populate ordered choices; metadata plots build for both the harmonized stage and the IDC class; categorical colour preparation yields the correct biologically-ordered levels; gene annotation yields 6,860 genes; a filter + gene-expression + `build_plot_slim()` round trip aligns correctly; an impossible stage combination is reported.

No browser-testing stack was installed and no long-running server was left running.

**Environment caveat:** `writexl` (used only by the Excel download handler in `server.R`) is not installed in either the `renv` library or the repo's pixi environment. For the smoke test only, `library(writexl)` was shimmed in the *test harness*, not in the app. This is a **pre-existing environment gap**, not something introduced here, but it means the Excel download path could not be exercised. **Project-lead action:** restore `renv` (`renv::restore()`) before deployment so `writexl` is available.

## 21. Versions used for validation

R 4.5.2; shiny 1.11.1, plotly 4.11.0, DT 0.34.0, dplyr 1.1.4, readr 2.1.5, yaml 2.3.10, memoise 2.0.1, bslib 0.9.0, fontawesome 0.5.3, shinyjs 2.1.0, RColorBrewer 1.1.3, stringr 1.6.0, conflicted 1.2.0, later 1.4.4.

(The repo's pixi environment launcher carried a stale absolute `R_HOME` from a different machine path; it was corrected in the untracked `.pixi/` directory so the environment could be used. `.pixi/` is not part of the repository.)

## 22. Known limitations (all non-blocking)

1. **Bibliography has no PMID column** -> the D067 PMID cross-check is reported as WARN rather than performed. Recommend adding `study_pmid` upstream.
2. **`normalize_df.rds` / `raw_df.rds` are not in `build_manifest.tsv`** -> their hashes are recorded here instead.
3. **`writexl` missing from the runtime environment** -> Excel download path untested; `renv::restore()` needed before deployment.
4. **21 cells (7 Hazzard2024, 14 Mancio-Silva2022) have NA in both stage fields.** `metadata_schema.tsv` declares both fields non-nullable, so this is a minor upstream deviation. The app handles it correctly and consistently (both fields missing together, displayed as "Missing"), and the validator asserts the pairing. **Flagged for the project lead** as an upstream question, not an app defect.
5. **`technology` and `strain` columns are sparse** and were superseded by `sc_technology` / `parasite_lineage_isolate` for app use (see section 3). They remain in the data.
6. **Cosmetic:** the metadata tab now has three rows of cards instead of two. The `scale_df.rds` export is still not loaded by the app (unchanged from before).
7. **`adjudicated_display_stage` is intentionally not exposed** pending DEC07.
8. The nine files in `docs/legacy/` still contain obsolete column names by design; they are excluded from the AT20 live scan and carry an explicit historical header.

## 23. Release-candidate criteria

| Blocking condition | Result |
|---|---|
| App loads against Phase-2 candidate data | PASS |
| All configured fields resolve | PASS (40/40) |
| AT19 passes | PASS |
| AT20 passes | PASS |
| Filters work against the new schema | PASS (8/8 populate; valid combination returns rows) |
| Broad/detailed lifecycle filtering valid | PASS (constrained + explicit message) |
| IDC field has correct semantics | PASS |
| Source/inferred gametocyte provenance not collapsed | PASS |
| Bibliography mapping valid | PASS (PMID check WARN, documented) |
| Expression tables align | PASS |
| No obsolete schema assumption causes a live code failure | PASS |
| Candidate data untouched | PASS |
| Existing deployed app data not overwritten | PASS |
| No major startup/runtime error remains | PASS |

**All blocking conditions met -> PHASE-3 APP RELEASE CANDIDATE.**

## 26. Final validation against the preprocessing release-closure candidate (2026-09-21)

**Status: FINAL APP RELEASE CANDIDATE** (branch `plavisca-app-production-repair`, not merged, not deployed).

**Preprocessing closure evidence.** Branch `plavisca-release-closure` (`pre_process_data` repo), exact remote SHA `dea656faa2b06381cc971b462de8f911df1028f7`, built on Phase-2 scientific commit `54c6b9581ff5268422ddffda42ce457bf09c5b55`. Its `REPORT.md`/`final_release_readiness.tsv` verdict: **FINAL RELEASE DATA CANDIDATE** — 29/29 technically-actionable AT01–AT30 criteria PASS, 0 FAIL, 1 PENDING_DECISION (AT14, Mancio DEC01/DEC02 — explicitly non-blocking: no cell membership changed to force closure, and `FINAL_MANIFEST.md` states `final_release_manifest.tsv` is authoritative for "the PlaViSca v1 data release candidate"). Authoritative manifest: `pre_process_data/audit/phase3_preflight/final_release_manifest.tsv`.

**Hash verification (mandatory, done before any test ran).** All required app inputs matched the authoritative manifest exactly: `cleaned_dataset.rds`, `normalize_df.rds`, `raw_df.rds`, `data_source.csv`, `scale_df.rds`, `pv_all_studies.rds`. See `candidate_data_manifest.tsv` for old vs. final bytes. Only `cleaned_dataset.rds` (AT17 row-order canonicalization) and `data_source.csv` (added `study_pmid`) actually changed; `normalize_df.rds`/`raw_df.rds`/`scale_df.rds`/`pv_all_studies.rds` are byte-identical to the first-pass Phase-3 candidate.

**App code changes since `c44cde0`: none.** Only audit artefacts were added/updated (this section, `candidate_data_manifest.tsv`, `final_candidate_validation.tsv`, `legacy_relocation_review.tsv`, `test_results.tsv`, `app_contract_validation.tsv`). No `functions/`, `global.R`, `server.R`, `ui.R`, `config/` file changed, because none needed to: the app already resolves data purely through `PLAVISCA_DATA_DIR`/keyed joins and required no positional-order assumption fix.

**Full test suite:** 68 run / **67 PASS / 0 FAIL / 1 SKIP** (previous baseline: 68/68/0/0). The one skip (`meta_plotly` not loaded) and one AT19/AT20-adjacent gap are both explained by a **host R-environment regression** unrelated to the data or app code — see the environment note below.

**Startup contract (re-run from scratch, standalone):** 33 checks / **33 PASS / 0 WARN / 0 FAIL** (previous baseline: 32 PASS/1 WARN/0 FAIL). The prior WARN (`data_source.csv` had no PMID column, so `bibliography_pmid_consistency` could only be a WARN) is now a hard PASS: `study_pmid` is present, and the existing (unmodified) code in `functions/data_contract.R` automatically upgrades this check to a real PASS/FAIL gate whenever a PMID column is available — no code change was required to satisfy the "PMID mismatch must be a startup failure, not a warning" instruction.

**AT17 (app-side, closed):** `cleaned_dataset.rds$mr_data`, `normalize_df.rds`, `raw_df.rds`, and `scale_df.rds` now all share identical row order across all 105,963 cells, and that order is lexicographic by cell key — matching the preprocessing closure's canonical order. Keyed joins were **not** relaxed to positional joins; the existing set-equality/keyed-alignment tests (`test_data_contract.R`) remain the regression guard against positional corruption, exactly as before.

**AT19 / AT20:** both PASS (`tests/test_at19_at20.R`, part of the 67/68 run above) — all 40 configured fields resolve to real current-schema columns; no live code references obsolete pre-Phase-2 names.

**Legacy relocation review (independent re-check):** see `legacy_relocation_review.tsv`. All nine files moved to `docs/legacy/` in Phase-3 were independently re-verified by grepping every symbol they define against `global.R`/`server.R`/`ui.R`/`app.R`: zero live references. Verdicts: `SAFE_DEAD_CODE` (3) / `SAFE_REPLACED` (6). **No BLOCKER.**

**`sc_technology` vs `technology` (retained):** `sc_technology` is 105,963/105,963 non-NA across all 6 studies (3 distinct values); `technology` is 80,024/105,963 non-NA, 1 distinct value, Hazzard2024 only. `sc_technology` remains strictly the more complete and semantically correct field; retained, no change.

**`parasite_lineage_isolate` vs `strain` (retained):** `parasite_lineage_isolate` is 99,737/105,963 non-NA (Hazzard2024/Ruberto2022_2/Sa2020), documented in the preprocessing metadata schema as the keyed, corrected strain/isolate field. The legacy `strain` column (25,939/105,963, complementary study coverage) has no schema entry and is not referenced anywhere in live app code. `parasite_lineage_isolate` remains the correct app-facing concept; retained, no change.

**Missing-stage cells (unchanged, still correctly handled):** exactly 21 cells (7 Hazzard2024, 14 Mancio-Silva2022) have both `parasite_broad_stage` and `harmonized_life_cycle_stage` NA together — matching the preprocessing closure's own `stage_missing_validation.tsv` exactly. No case has only one of the two fields NA. The app displays these as "Missing" and fabricates no stage.

**Bibliography PMID:** `data_source.csv` now carries one verified `study_pmid` per study, in exact agreement with the atlas. This is enforced as a hard startup failure condition (see startup-contract note above), not a warning.

**`writexl` / Excel export — non-blocking limitation, unresolved and now worse than the Phase-3 baseline.** `writexl` is still absent from the `renv` library. `renv::restore()` was attempted (network access confirmed available) and **failed outright**: the pinned `stringi` source will not compile against this machine's R 4.6.1 (the C++ code uses the `LEVELS` macro, removed from R's C API after the lockfile's target R 4.3.1 — this is a host R-version regression since the Phase-3 baseline, which ran R 4.5.2 with a working library). Excel export was **not** tested. **Deployment prerequisite:** restore a compatible R version (≤4.5.x, matching the last known-working baseline) or refresh the pinned lockfile before deployment.

**Full source/startup smoke test — partial, documented limitation.** `global.R`/`ui.R`/`server.R`/`app.R` all parse with zero syntax errors. The live `shinyApp` object could **not** be constructed on this machine: `shiny`, `dplyr`, `plotly`, `conflicted` and other runtime packages are absent from the `renv` library, for the same root cause as the `writexl` gap above (this is a broader package-availability regression than the Phase-3 baseline, not a new code or data defect). Indirect evidence the app still starts correctly against the final data: AT20's static check that `global.R` sources only existing files (PASS), and the embedded "full startup contract validation passes with no FAILs" test (PASS), which exercises the identical contract logic the live app runs at startup, directly against the final candidate bytes. Excel-export and full Shiny-render exercising were **not** claimed as tested — see `final_candidate_validation.tsv`.

**Ruberto2022_1 feature-availability note:** verified still present verbatim in `config/data_columns.yml` (2,089-feature gap). No UI work reopened.

**Remaining blockers:** none.

**Remaining non-blocking limitations:** (1) `writexl`/Excel export untested pending `renv::restore()` on a compatible R version; (2) full live Shiny smoke test not executable on this machine for the same environment reason; (3) AT14 (Mancio DEC01/DEC02) pending explicit project-lead sign-off, carried from preprocessing closure, non-blocking for this v1 candidate per the closure report's own framing.

See `final_candidate_validation.tsv` for the itemized check table.

## 24. Handover — requires project-lead review before merge/deployment

1. **Not merged to `master`, not deployed.** Branch `plavisca-app-production-repair`.
2. ~~Decide whether `sc_technology` (used) vs `technology` (sparse) and `parasite_lineage_isolate` (used) vs `strain` (sparse) are the intended app-visible fields~~ — **resolved in section 26**: both retained, confirmed correct against final data.
3. ~~Add `study_pmid` to the bibliography export~~ — **done in preprocessing closure**; PMID consistency is now a hard startup-failure gate, not a warning.
4. Confirm the 21 dual-NA stage cells are acceptable, or repair upstream — still present unchanged in the final candidate, see section 26.
5. Run `renv::restore()` so `writexl` is installed before deployment — **still required**; now fails outright on this machine's R 4.6.1 (see section 26), needs a compatible R version.
6. Review the removal of the nine unreferenced module files to `docs/legacy/` — section 12; independently re-verified in section 26, no blocker.
7. Several of the 88 metadata fields are not surfaced anywhere in the UI (`run_id`, `sample_type`, `host_id`, `host_sex`, `host_taxid`, `per_cell_parasite_genotype`, the Mancio-only verbatim `source_*` fields, the `dot1_*`/`d5_*` duplicate flags, `main_analysis_member`, `retained_by_plavisca_qc`, the `legacy_starsolo_*` fields, `seurat_clusters`, `infection_design`, `source_run_count_*`, `source_cell_id`, `reconstructed_barcode`, `count_source_rds_*`, `total_umi_count`, `near_empty_expression_flag`). Several are deliberately internal per `metadata_schema.tsv` (`app_visible = No`); confirm none of the remainder are wanted in v1.

## 25. Files in this audit directory

| File | Contents |
|---|---|
| `REPORT.md` | this document |
| `app_contract_validation.tsv` | the 33 startup contract checks with status and detail |
| `test_results.tsv` | all 68 tests with file, name, status |
| `candidate_data_manifest.tsv` | resolved paths, sizes and SHA256 of every candidate artefact used |
| `obsolete_schema_scan.tsv` | every occurrence of an obsolete schema token across live and legacy files, with an `at20_violation` flag (0 violations) |
| `bibliography_validation.tsv` | the bibliography check results |
| `bibliography_study_join.tsv` | the six-study join between atlas metadata and `data_source.csv` |
| `field_mapping.tsv` | AT19 evidence: every configured field, its target column, presence in both tables, missingness and cardinality |
| `stage_compatibility.tsv` | the detailed-to-broad mapping with per-stage verification against the candidate |
| `final_candidate_validation.tsv` | final-release validation checklist against preprocessing closure SHA `dea656f` (section 26) |
| `legacy_relocation_review.tsv` | independent second review of the nine `docs/legacy/` relocations (section 26) |
