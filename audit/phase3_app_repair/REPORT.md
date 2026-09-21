# PlaViSca Phase-3 App Repair — Audit Report

**Status: FINAL APP RELEASE CANDIDATE — DEC01/DEC02 RESOLVED** (updated 2026-09-21 — see section 27)
(not merged to `master`, not deployed — see *Handover* at the end)

Section 26 below documents an earlier pass against the `plavisca-release-closure`
candidate (`dea656f`, 105,963 cells, AT14/DEC01-DEC02 still `PENDING_DECISION`).
**That candidate is now superseded.** Section 27 documents validation against the
authoritative, DEC01/DEC02-resolved final candidate (`12d2e440`, 105,949 cells,
30/30 acceptance PASS, 0 PENDING) and is the current release verdict.

| Item | Value (current, section 27) |
|---|---|
| App repo | `PlaViSca` |
| App base commit | `020f9cbd651750ba92c037399fb84d3b17d389e8` (`master`, "simplify README more") |
| App branch | `plavisca-app-production-repair` |
| Preprocessing repo commit consumed (superseded, first pass) | `54c6b9581ff5268422ddffda42ce457bf09c5b55` (branch `plavisca-production-repair`) |
| Preprocessing closure SHA (superseded) | `dea656faa2b06381cc971b462de8f911df1028f7` (branch `plavisca-release-closure`, 105,963 cells) — see section 26 |
| **Preprocessing final SHA (authoritative, current)** | **`12d2e44027439ab5c88007fe14711fa6c80a6d69`** (branch `plavisca-dec01-dec02-final`) — see section 27 |
| Preprocessing acceptance (current) | **30 PASS / 0 FAIL / 0 PENDING** — "FINAL RELEASE DATA CANDIDATE — DEC01/DEC02 RESOLVED" |
| Candidate export path used for validation | `pre_process_data_dec01_dec02_final/data/candidate_export/` (+ `.../data/data_source.csv`), reached through a symlink staging directory outside both repos |
| Atlas size validated (current) | **105,949** cells x 85 metadata fields x 6,860 genes, 6 studies |
| Tests (current final candidate) | 74 run / **74 passed** / 0 failed / 0 skipped — see section 27 |
| Startup contract checks (current final candidate) | 33 run / **33 PASS** / 0 WARN / 0 FAIL |
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

**Everything above this point in section 26 describes the release-closure candidate
(`dea656f`, 105,963 cells, AT14/DEC01-DEC02 `PENDING_DECISION`). That candidate is
historical and superseded by section 27.**

## 27. FINAL validation against the DEC01/DEC02-resolved release candidate (2026-09-21)

**Status: FINAL APP RELEASE CANDIDATE — DEC01/DEC02 RESOLVED.**

**Preprocessing authority.** Repo `pre_process_data`, branch `plavisca-dec01-dec02-final`,
commit `12d2e44027439ab5c88007fe14711fa6c80a6d69` (scientific rebuild commit
`173431cfca8e19c716a96164ee2ea6053441855b`). Acceptance matrix
`audit/phase3_preflight/acceptance_matrix_AT01_AT30.tsv`: **30/30 PASS, 0 FAIL,
0 PENDING**. AT25 was resolved upstream as a validation-semantics defect (no
scientific data file changed for that fix); AT25 is not reopened here. Authoritative
manifest: `pre_process_data/audit/phase3_preflight/final_release_manifest.tsv`
(independently re-checked against this manifest below, not just against the
app-side `candidate_data_manifest.tsv`).

**DEC01.** Exactly the 14 proven `.1` Mancio-Silva2022 duplicate representations
listed in `pre_process_data/audit/mancio_silva2022/dec01_removed_duplicate_cells.tsv`
are absent from the final candidate, and their 14 unsuffixed counterparts are
present. Verified directly against `cleaned_dataset.rds` (not by re-deriving the ID
list) in `tests/test_final_release_candidate.R` — PASS.

**DEC02.** Both `D5Seq1_CCCCGATTGACG` and `D5Seq2_CCCCGATTGACG` are present, trace
to 2 distinct `run_id` values (distinct sample provenance), and no
duplicate/suspicion column exists anywhere in the app-facing metadata. Verified in
`tests/test_final_release_candidate.R` — PASS.

**Six-study membership and total.** Exact per the authoritative manifest and
independently recomputed from `cleaned_dataset.rds$mr_data`:

| Study | Cells |
|---|---:|
| Mancio-Silva2022 | 1,480 |
| Sa2020 | 9,766 |
| Hazzard2022 | 3,294 |
| Hazzard2024 | 80,024 |
| Ruberto2022_1 | 1,438 |
| Ruberto2022_2 | 9,947 |
| **Total** | **105,949** |

**Hash verification (independently recomputed with `sha256sum`, not copied from any
prior audit file).** All six required files match the task-authoritative and
`final_release_manifest.tsv` values exactly:

| File | SHA256 |
|---|---|
| `pv_all_studies.rds` (atlas) | `32068878fc92952e9be539befcc8ff2f42a6818716a912a55f5c45930cc830b9` |
| `normalize_df.rds` | `e3afc56e52dc8eab940ac26777c17a9b27d19f680f7503bdbc373e1543c978f1` |
| `raw_df.rds` | `0c01491b0628abd651513eee265b9faae8b10da57ac838c292beffd6612170f1` |
| `scale_df.rds` | `a00f5a758fc09ee24193b993e673042f9d673ba1475f9378585891a7382fb505` |
| `cleaned_dataset.rds` | `c1a04525424b7bad5bc723b8b9862ff71bebd16016d860a8ce6e652ce5117fed` |
| `data_source.csv` (bibliography) | `e70f880575f84b070a97a53b6245afdedc7424e6ee7283efc146d11197492e09` |

See the rebuilt `candidate_data_manifest.tsv` for the resolved paths and a note on
the prior (now-superseded) hash set recorded during the release-closure pass.

**Staging.** `PLAVISCA_DATA_DIR` was pointed at a symlink-only staging directory
(`/tmp/plavisca_app_final.OpePL4`, outside the repo, not committed) whose targets
resolve into a dedicated worktree of the preprocessing repo checked out at
`plavisca-dec01-dec02-final`@`12d2e440`. No candidate binary or machine-specific
symlink is committed to this repo; `data/*` remains gitignored as before.

**App code/config changes required and made for this candidate:**
- `config/data_columns.yml`: `expected_cell_count` updated `105963 → 105949` (only
  place in active config carrying the total; searched the full active tree for
  `105963`/`1494` — no other current-release reference found; `REPORT.md` section
  26 above retains the old number, explicitly labeled historical).
- `tests/run_tests.R`: registered the new `tests/test_final_release_candidate.R`
  file in the suite.
- `tests/test_final_release_candidate.R` (new): asserts the exact 105,949 total and
  six per-study counts, DEC01 removal/retention, DEC02 retention with distinct
  provenance and no suspicion column, the 7 paired missing-stage cells, the IDC/
  gametocyte totals, and identical canonical cell-key order/uniqueness across all
  four cell-indexed exports.
- `tests/helper.R` and `functions/data_columns_config.R`: fixed a real ordering bug
  — both files defined the `%||%` null-coalescing operator *after* using it at
  module-load time. This is silently masked under R ≥ 4.4 (which has a base
  `%||%`) but is a hard startup crash under this project's pinned runtime, R 4.3.3
  (`could not find function "%||%"`, discovered while running the live Shiny
  construction smoke test below — the app could not start at all under its own
  designated R version before this fix). Fix: moved each `%||%` definition above
  its first use; no behavioral change otherwise. (A third occurrence in
  `functions/helpers_server.R` is inside a function body evaluated only after the
  whole file is sourced, so it was not affected and was left as-is.)

No other `functions/`, `global.R`, `server.R`, or `ui.R` change was needed or made;
DEC01/DEC02 resolution required no new app logic because the app already resolves
membership purely through keyed data, not positional or hardcoded ID assumptions.

**Environment used for this validation.** Conda env `plavisca-r433`
(`/home/baia/users/giorgio/software/mambaforge/envs/plavisca-r433`), R 4.3.3,
platform `x86_64-conda-linux-gnu` — this exactly matches the platform tag of this
repo's own `renv/library/R-4.3/x86_64-conda-linux-gnu` snapshot, i.e. it is the
project's actual designated runtime (not an ad hoc substitute). The host system R
(4.6.1, platform `x86_64-pc-linux-gnu`) lacks `dplyr`/`plotly`/`shiny`/`writexl`/`DT`
entirely and cannot load renv's R-4.3 binary packages (ABI mismatch); it was used
only for a first-pass sanity run. `R_LIBS_USER`/`R_LIBS` were explicitly unset
before invoking `plavisca-r433`'s `Rscript`, because this account's personal
`~/R-pkg` library (built for yet another R ABI) otherwise shadows the project's
`renv` library and crashes package loading (`yaml.so: undefined symbol:
R_ClosureBody`) — this is an account-level environment hazard, not a project
defect; documented here as a reproducibility prerequisite.

**Full test suite (independently rerun end-to-end after the fixes above, against
the exact hash-verified final candidate):**

```
TOTAL: 74   PASS: 74   FAIL: 0   SKIP: 0
```

74/74, 0 FAIL, 0 SKIP — an improvement over the section-26 baseline (68 run/67
pass/1 skip), because the `%||%` fix let `functions/helpers_server.R` load
cleanly under the project's real R version, which in turn let the previously
environment-skipped `meta_plotly`-dependent filter test actually run (and pass).
See `test_results.tsv`.

**Startup contract (re-run standalone via the live `global.R` load path, not just
the test-suite's embedded copy):**

```
PlaViSca data contract: 33/33 checks passed against '/tmp/plavisca_app_final.OpePL4'.
```

33/33, 0 WARN, 0 FAIL. See `app_contract_validation.tsv` for the itemized table
(cell count, six-study set, all 40 configured fields, unique/aligned cell keys, no
obsolete config targets, stage-pairing, IDC/gametocyte blood-only constraints,
embedding columns, expression-table alignment, and bibliography PMID
consistency — the latter a hard PASS/FAIL gate, not a WARN).

**AT19 / AT20:** both PASS, exercised as part of the 74/74 run
(`tests/test_at19_at20.R`). AT20 confirms no live app code references
`development_phase`, `parasite_stages`, obsolete `parasite_stage`/`life_cycle_stage`,
`hour_post_invasion`, `geographic_location`, or `blood_stage`; the nine
`docs/legacy/` files are excluded from the live-code scan by design and remain
unsourced (re-verified below).

**Cell-key / row-order alignment (AT17, app-side).** `cleaned_dataset.rds$mr_data`,
`normalize_df.rds`, `raw_df.rds`, and `scale_df.rds` all have exactly 105,949 rows,
an identical cell-key set, zero duplicate keys, and identical canonical
lexicographic order — asserted directly in
`tests/test_final_release_candidate.R` (not merely inspected) and independently
confirmed by the standalone rerun above.

**Missing-stage cells.** Exactly **7** cells (all Hazzard2024, all below the
10-UMI inference gate) have `parasite_broad_stage` and `harmonized_life_cycle_stage`
both NA together; no cell has only one of the two fields NA; no Mancio `.1`
stage-missing row remains (the DEC01-removed rows are gone entirely). This is down
from the 21-cell (7 Hazzard2024 + 14 Mancio `.1`) figure in the section-26 baseline
precisely because DEC01 removed the 14 Mancio duplicate rows that were contributing
to that count — expected and correct, not a new defect. `relabel_missing(NA)`
still renders the configured "Missing" label. Filtering/ordering logic does not
error on these rows (exercised by `test_lifecycle.R`/`test_bibliography_and_filters.R`
in the same run).

**IDC / gametocyte invariants (app-facing agreement with preprocessing totals):**
IDC labeled = **90,219**; gametocyte calls = **22,795**; 0 non-blood cells carry
either an IDC reference-time class or an inferred gametocyte sex (impossible
lifecycle states = 0). Source-reported and PlaViSca-inferred gametocyte sex remain
distinct, separately provenance-labeled columns; no scientific adjudication is
invented. Asserted in `tests/test_final_release_candidate.R` and
`app_contract_validation.tsv`.

**Bibliography.** `data_source.csv` (final, hash-verified above) has exactly 6
rows, one per atlas study, each with exactly one verified PMID, exact per-study
cell-count agreement with the atlas, and exact PMID agreement — all as hard
PASS/FAIL startup checks (`bibliography_no_duplicates`, `bibliography_covers_atlas`,
`bibliography_no_extra_studies`, `bibliography_cell_counts`,
`bibliography_pmid_consistency`, `atlas_one_pmid_per_study`), all PASS.

**`sc_technology` (retained, reverified):** unchanged decision; still the field of
full/near-full app-relevant coverage vs. the sparser `technology` column. No new
evidence contradicts the prior decision.

**`parasite_lineage_isolate` (retained, reverified):** unchanged decision for the
same reason as `sc_technology`; still preferred over legacy `strain`.

**Legacy relocation (`docs/legacy/`):** static/source re-check confirms none of the
nine files are sourced and no live function dependency was introduced by any
change in this pass (the only touched files are the four listed above, none of
which reference `docs/legacy/`). Full historical re-investigation not repeated —
reused the existing `legacy_relocation_review.tsv` evidence, which remains valid
because nothing under `functions/`/`global.R`/`server.R`/`ui.R` changed except the
`%||%` ordering fix.

**Ruberto2022_1 feature-availability note:** reverified present verbatim in
`config/data_columns.yml` (the v1 limitation note on the 2,089 PlaViSca features
structurally unavailable from the author-processed matrix). No new UI machinery
added.

**Shiny smoke test — full construction, not partial.** Using the `plavisca-r433`
environment above (which section 26 could not access), `global.R`, `ui.R`, and
`server.R` were sourced and `shiny::shinyApp(ui, server)` was constructed
end-to-end with zero errors:

```
PlaViSca data contract: 33/33 checks passed against '/tmp/plavisca_app_final.OpePL4'.
GLOBAL_PASS
UI_PASS
SERVER_SOURCED_PASS
SHINY_APP_CONSTRUCTION_PASS
```

No server was left running. See `shiny_smoke_test.log`.

**`writexl` / Excel export — now fully tested, resolved.** `library(writexl)`
loads cleanly in the project's designated R 4.3.3 environment, and a `write_xlsx()`
smoke export of live candidate metadata produced a valid non-empty `.xlsx` file.
The section-26 "writexl missing" and "renv::restore() fails on R 4.6.1" findings
were an artifact of validating against the wrong (system) R interpreter, not a
real package or code gap — the project's own pinned `renv` library for R 4.3.3
already carries a working `writexl`. See `writexl_export_test.log`.

**Remaining blockers:** none.

**Remaining non-blocking / deployment notes:** deployment must use the
`plavisca-r433` conda environment (R 4.3.3, `x86_64-conda-linux-gnu`) or an
equivalent R 4.3.3 build matching `renv/library/R-4.3/x86_64-conda-linux-gnu`; the
host system R (4.6.1) cannot load this project's compiled dependencies. Deploying
accounts should ensure `R_LIBS_USER`/`R_LIBS` do not point at an incompatible
personal library, for the reason documented above.

**Final app release criteria — all met:**

| Blocking condition | Result |
|---|---|
| Authoritative preprocessing commit (`12d2e440`) verified | PASS |
| All six required hashes match | PASS |
| Total = 105,949 | PASS |
| Exact six study counts | PASS |
| DEC01 verified | PASS |
| DEC02 verified | PASS |
| App tests, 0 FAIL | PASS (74/74) |
| Startup contract, 0 FAIL | PASS (33/33, 0 WARN) |
| AT19 | PASS |
| AT20 | PASS |
| Bibliography | PASS |
| Cell-key/order alignment | PASS |
| No newly introduced app regression | PASS (one pre-existing startup bug fixed, not introduced) |
| No candidate binary committed | PASS |
| No merge performed | confirmed |
| No deployment performed | confirmed |

**Designation: FINAL APP RELEASE CANDIDATE — DEC01/DEC02 RESOLVED.**

See `final_candidate_validation.tsv` (rebuilt) and `candidate_data_manifest.tsv`
(rebuilt) for the itemized, re-verifiable evidence backing this section.

## 24. Handover — requires project-lead review before merge/deployment

**Superseded by section 27 (read that section first):** item 4 below (21 dual-NA
cells) is now 7, entirely Hazzard2024, because DEC01 removed the 14 Mancio `.1`
rows that made up the rest — not a new defect. Item 5 (`writexl`) is resolved:
the earlier failure was validation against the wrong R interpreter, not a real
package gap — see section 27's environment note. Deployment must use the
`plavisca-r433` R 4.3.3 environment described there.

1. **Not merged to `master`, not deployed.** Branch `plavisca-app-production-repair`.
2. ~~Decide whether `sc_technology` (used) vs `technology` (sparse) and `parasite_lineage_isolate` (used) vs `strain` (sparse) are the intended app-visible fields~~ — **resolved in section 26**: both retained, confirmed correct against final data.
3. ~~Add `study_pmid` to the bibliography export~~ — **done in preprocessing closure**; PMID consistency is now a hard startup-failure gate, not a warning.
4. Confirm the 7 dual-NA stage cells (all Hazzard2024, all sub-10-UMI) are acceptable — see section 27; this is expected low-UMI-driven missingness, not an app defect.
5. ~~Run `renv::restore()` so `writexl` is installed before deployment~~ — **resolved in section 27**: `writexl` already works in the project's own `plavisca-r433` R 4.3.3 environment; no `renv::restore()` needed, just the correct interpreter.
6. Review the removal of the nine unreferenced module files to `docs/legacy/` — section 12; independently re-verified in section 26, no blocker.
7. Several of the 88 metadata fields are not surfaced anywhere in the UI (`run_id`, `sample_type`, `host_id`, `host_sex`, `host_taxid`, `per_cell_parasite_genotype`, the Mancio-only verbatim `source_*` fields, the `dot1_*`/`d5_*` duplicate flags, `main_analysis_member`, `retained_by_plavisca_qc`, the `legacy_starsolo_*` fields, `seurat_clusters`, `infection_design`, `source_run_count_*`, `source_cell_id`, `reconstructed_barcode`, `count_source_rds_*`, `total_umi_count`, `near_empty_expression_flag`). Several are deliberately internal per `metadata_schema.tsv` (`app_visible = No`); confirm none of the remainder are wanted in v1.

## 25. Files in this audit directory

| File | Contents |
|---|---|
| `REPORT.md` | this document |
| `app_contract_validation.tsv` | the current 33 startup contract checks (final candidate, section 27) with status and detail |
| `test_results.tsv` | the current 74 tests (final candidate, section 27) with file, name, status |
| `candidate_data_manifest.tsv` | resolved paths, sizes and SHA256 of every candidate artefact used, current final candidate (section 27) plus the superseded section-26 hash set for reference |
| `obsolete_schema_scan.tsv` | every occurrence of an obsolete schema token across live and legacy files, with an `at20_violation` flag (0 violations) |
| `bibliography_validation.tsv` | the bibliography check results |
| `bibliography_study_join.tsv` | the six-study join between atlas metadata and `data_source.csv` |
| `field_mapping.tsv` | AT19 evidence: every configured field, its target column, presence in both tables, missingness and cardinality |
| `stage_compatibility.tsv` | the detailed-to-broad mapping with per-stage verification against the candidate |
| `final_candidate_validation.tsv` | current final-release validation checklist against preprocessing SHA `12d2e440` (section 27), with the superseded `dea656f`-era rows (section 26) kept and explicitly labeled historical |
| `legacy_relocation_review.tsv` | independent second review of the nine `docs/legacy/` relocations (section 26; reused unchanged in section 27, see note there) |
| `shiny_smoke_test.log` | full `global.R`/`ui.R`/`server.R`/`shinyApp()` construction, 0 errors (section 27) |
| `writexl_export_test.log` | live `write_xlsx()` smoke export against final candidate metadata (section 27) |
