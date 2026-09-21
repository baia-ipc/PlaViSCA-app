# Startup data-contract validation for the Phase-2 atlas export.
# =============================================================================
# The app must fail loudly and early - never silently continue - when the data
# it is pointed at does not match what the code expects. This module implements
# the Phase-3 startup validator.
#
# It is deliberately independent of Shiny so it can be exercised directly from
# the test suite.
# =============================================================================

#' Resolve an application data file path from PLAVISCA_DATA_DIR.
#'
#' All application data paths are constructed from this single function so the
#' app can be pointed at a candidate/staging export without editing any code
#' and without any absolute path ever appearing in the source.
app_data_dir <- function() {
    Sys.getenv("PLAVISCA_DATA_DIR", unset = "data")
}

app_data_path <- function(filename) {
    file.path(app_data_dir(), filename)
}

APP_DATA_FILES <- list(
    cleaned_dataset = "cleaned_dataset.rds",
    norm_df = "normalize_df.rds",
    raw_df = "raw_df.rds",
    data_source = "data_source.csv"
)

#' Build the list of resolved data paths used by the app.
app_data_paths <- function() {
    lapply(APP_DATA_FILES, app_data_path)
}

.contract_row <- function(check, status, detail) {
    data.frame(check = check, status = status, detail = detail, stringsAsFactors = FALSE)
}

#' Validate the loaded atlas against the app's data contract.
#'
#' @param mr_data     cleaned_dataset.rds$mr_data
#' @param expr_df     the normalized (or raw) wide table, metadata + genes
#' @param bibliography data_source.csv as a data.frame, or NULL to skip
#' @param expr_label  label used in messages for `expr_df`
#' @return a data.frame of check/status/detail rows. Status is "PASS", "FAIL"
#'         or "WARN".
validate_app_data_contract <- function(mr_data,
                                       expr_df = NULL,
                                       bibliography = NULL,
                                       expr_label = "normalize_df") {
    rows <- list()
    add <- function(check, ok, detail, warn_only = FALSE) {
        status <- if (ok) "PASS" else if (warn_only) "WARN" else "FAIL"
        rows[[length(rows) + 1]] <<- .contract_row(check, status, detail)
    }

    cols <- colnames(mr_data)

    # --- 1. cell count -------------------------------------------------------
    expected_n <- app_expected_cell_count()
    add(
        "cell_count",
        nrow(mr_data) == expected_n,
        sprintf("expected %d metadata rows, found %d", expected_n, nrow(mr_data))
    )

    # --- 2. every required configured field resolves --------------------------
    # This runs before any check that dereferences a configured column, so
    # incompatible (e.g. pre-Phase-2) data produces an actionable schema
    # message rather than an obscure subsetting error deeper down.
    config_problems <- validate_data_columns(mr_data, stop_on_error = FALSE)
    add(
        "configured_fields_resolve",
        length(config_problems) == 0,
        if (length(config_problems) == 0) {
            sprintf("all %d required configured fields present", length(required_field_keys()))
        } else {
            paste(config_problems, collapse = "; ")
        }
    )
    if (length(config_problems) > 0) {
        add(
            "schema_compatibility",
            FALSE,
            paste(
                "the loaded dataset does not carry the Phase-2 atlas schema this",
                "version of the app requires; remaining checks were skipped.",
                "Point PLAVISCA_DATA_DIR at a Phase-2 export."
            )
        )
        out <- do.call(rbind, rows)
        rownames(out) <- NULL
        return(out)
    }

    # --- 3. studies ----------------------------------------------------------
    expected_studies <- app_expected_studies()
    observed_studies <- sort(unique(as.character(mr_data[[get_data_column("study_label")]])))
    add(
        "study_set",
        setequal(observed_studies, expected_studies),
        sprintf(
            "expected [%s]; found [%s]",
            paste(sort(expected_studies), collapse = ", "),
            paste(observed_studies, collapse = ", ")
        )
    )

    # --- 4. no configured field maps to an obsolete column --------------------
    obsolete <- app_obsolete_columns()
    obsolete_targets <- Filter(
        function(k) get_data_column(k) %in% obsolete,
        data_field_keys()
    )
    add(
        "no_obsolete_config_targets",
        length(obsolete_targets) == 0,
        if (length(obsolete_targets) == 0) {
            "no configured field points at a pre-Phase-2 column"
        } else {
            paste(obsolete_targets, collapse = ", ")
        }
    )

    # --- 5. obsolete physical columns are not required -------------------------
    present_obsolete <- intersect(obsolete, cols)
    add(
        "obsolete_columns_not_required",
        length(intersect(present_obsolete, required_data_columns())) == 0,
        sprintf(
            "obsolete columns still present in data (informational): [%s]",
            paste(present_obsolete, collapse = ", ")
        )
    )

    # --- 6. cell identifiers ---------------------------------------------------
    barcode_col <- get_data_column("barcode")
    add(
        "unique_cell_ids",
        !any(duplicated(mr_data[[barcode_col]])),
        sprintf("%d duplicate cell ids", sum(duplicated(mr_data[[barcode_col]])))
    )
    add(
        "rownames_match_cell_ids",
        identical(rownames(mr_data), as.character(mr_data[[barcode_col]])),
        "metadata rownames must equal the barcode column"
    )

    # --- 7. no literal "NA" categories in app-visible categorical fields -------
    visible_keys <- unique(c(filter_field_keys(), category_field_keys()))
    offenders <- character(0)
    for (key in visible_keys) {
        col <- get_data_column(key)
        if (!col %in% cols) next
        v <- mr_data[[col]]
        if (!(is.character(v) || is.factor(v))) next
        chr <- as.character(v)
        hits <- sum(!is.na(chr) & chr %in% LITERAL_MISSING_TOKENS)
        if (hits > 0) offenders <- c(offenders, sprintf("%s (%d)", col, hits))
    }
    add(
        "no_literal_na_categories",
        length(offenders) == 0,
        if (length(offenders) == 0) {
            "no literal 'NA'/'None' strings in app-visible categorical fields"
        } else {
            paste(offenders, collapse = ", ")
        }
    )

    # --- 8. broad / detailed stage compatibility -------------------------------
    broad_col <- get_data_column("broad_stage")
    detailed_col <- get_data_column("detailed_stage")
    map <- stage_compatibility_map()
    observed <- unique(mr_data[, c(detailed_col, broad_col)])
    observed <- observed[!is.na(observed[[detailed_col]]) & !is.na(observed[[broad_col]]), , drop = FALSE]
    unknown_detailed <- setdiff(observed[[detailed_col]], names(map))
    mismatched <- observed[
        observed[[detailed_col]] %in% names(map) &
            map[observed[[detailed_col]]] != observed[[broad_col]], ,
        drop = FALSE
    ]
    add(
        "stage_mapping_vocabulary",
        length(unknown_detailed) == 0,
        if (length(unknown_detailed) == 0) {
            "every observed detailed stage is in the configured mapping"
        } else {
            sprintf("detailed stage(s) missing from config: %s", paste(unknown_detailed, collapse = ", "))
        }
    )
    add(
        "stage_mapping_consistent",
        nrow(mismatched) == 0,
        if (nrow(mismatched) == 0) {
            "every detailed stage maps to its configured broad stage in the data"
        } else {
            paste(sprintf(
                "%s -> %s (config says %s)",
                mismatched[[detailed_col]], mismatched[[broad_col]],
                map[mismatched[[detailed_col]]]
            ), collapse = "; ")
        }
    )
    # broad and detailed must be missing together, never one without the other
    inconsistent_na <- sum(xor(is.na(mr_data[[broad_col]]), is.na(mr_data[[detailed_col]])))
    add(
        "stage_missingness_paired",
        inconsistent_na == 0,
        sprintf("%d cells have one stage field missing but not the other", inconsistent_na)
    )

    # --- 9. IDC fields present, old HPI not required ---------------------------
    idc_col <- get_data_column("idc_reference_class")
    add(
        "idc_fields_present",
        all(c(idc_col, get_data_column("idc_heuristic_stage_bin"),
            get_data_column("idc_similarity_raw_label"),
            get_data_column("idc_similarity_score_delta"),
            get_data_column("idc_similarity_pruned_flag")) %in% cols),
        "IDC reference-similarity fields and their uncertainty companions exist"
    )
    add(
        "hour_post_invasion_not_required",
        !("hour_post_invasion" %in% required_data_columns()),
        "the app must not require the obsolete hour_post_invasion column"
    )
    # IDC classification must only exist for blood-eligible cells
    tissue_col <- get_data_column("tissue_or_sample_type")
    leaked <- sum(!is.na(mr_data[[idc_col]]) & mr_data[[tissue_col]] != "Host blood")
    add(
        "idc_blood_only",
        leaked == 0,
        sprintf("%d non-blood cells carry an IDC reference-time class", leaked)
    )

    # --- 10. gametocyte provenance not collapsed -------------------------------
    pred_col <- get_data_column("pred_gametocyte_sex")
    src_col <- get_data_column("source_sex_annotation")
    add(
        "gametocyte_provenance_distinct",
        pred_col != src_col && all(c(pred_col, src_col) %in% cols),
        "inferred and author-reported gametocyte sex are separate columns"
    )
    gam_leak <- sum(!is.na(mr_data[[pred_col]]) & mr_data[[tissue_col]] != "Host blood")
    add(
        "inferred_sex_blood_only",
        gam_leak == 0,
        sprintf("%d non-blood cells carry a PlaViSca-inferred gametocyte sex", gam_leak)
    )
    if ("adjudicated_display_stage" %in% cols) {
        add(
            "adjudication_not_invented",
            all(is.na(mr_data$adjudicated_display_stage)),
            "adjudicated_display_stage is unresolved by design (DEC07) and must stay empty"
        )
    }

    # --- 11. embeddings --------------------------------------------------------
    for (prefix in c("umap_u_", "umap_i_", "pca_u_", "pca_i_", "t_sne_")) {
        found <- grep(paste0("^", prefix, "\\d+$"), cols, value = TRUE)
        add(
            paste0("embedding_", sub("_$", "", prefix)),
            length(found) >= 2,
            sprintf("found %d %s* columns", length(found), prefix)
        )
    }

    # --- 12. expression table alignment ----------------------------------------
    if (!is.null(expr_df)) {
        expr_cols <- colnames(expr_df)
        add(
            paste0(expr_label, "_row_count"),
            nrow(expr_df) == nrow(mr_data),
            sprintf("%d expression rows vs %d metadata rows", nrow(expr_df), nrow(mr_data))
        )
        add(
            paste0(expr_label, "_cell_keys"),
            setequal(rownames(expr_df), rownames(mr_data)),
            "expression and metadata must describe the same set of cells"
        )
        add(
            paste0(expr_label, "_no_duplicate_cells"),
            !any(duplicated(rownames(expr_df))),
            sprintf("%d duplicate expression rownames", sum(duplicated(rownames(expr_df))))
        )
        missing_meta <- setdiff(required_data_columns(), expr_cols)
        add(
            paste0(expr_label, "_carries_metadata"),
            length(missing_meta) == 0,
            if (length(missing_meta) == 0) {
                "expression table carries every required metadata column"
            } else {
                paste(missing_meta, collapse = ", ")
            }
        )
        gene_cols <- identify_gene_columns(expr_df, mr_data)
        add(
            paste0(expr_label, "_gene_identification"),
            length(gene_cols) > 1000 && !any(gene_cols %in% colnames(mr_data)),
            sprintf(
                "%d gene columns identified by identifier, %d metadata columns excluded",
                length(gene_cols), ncol(expr_df) - length(gene_cols)
            )
        )
    }

    # --- 13. bibliography ------------------------------------------------------
    if (!is.null(bibliography)) {
        bib <- validate_bibliography(mr_data, bibliography)
        rows <- c(rows, split(bib, seq_len(nrow(bib))))
    }

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

#' Identify the gene-expression columns of a wide expression table.
#'
#' Genes are identified by *identifier*, never by a positional column range:
#' the Phase-2 metadata block grew from ~52 to 88 columns and any positional
#' assumption would now silently treat metadata as genes.
identify_gene_columns <- function(expr_df, mr_data = NULL, pattern = "^PVP01_") {
    cand <- grep(pattern, colnames(expr_df), value = TRUE, ignore.case = TRUE)
    meta_cols <- if (!is.null(mr_data)) colnames(mr_data) else character(0)
    setdiff(cand, meta_cols)
}

#' Validate that the atlas' studies and the bibliography table agree exactly.
#'
#' Closes the D056/D057/D067 defect family: the bibliography was previously a
#' hand-maintained table with no join back to the metadata, so drift between
#' the two was undetectable.
validate_bibliography <- function(mr_data, bibliography) {
    rows <- list()
    add <- function(check, ok, detail, warn_only = FALSE) {
        status <- if (ok) "PASS" else if (warn_only) "WARN" else "FAIL"
        rows[[length(rows) + 1]] <<- .contract_row(check, status, detail)
    }

    study_col <- get_data_column("study_label")
    atlas_studies <- sort(unique(as.character(mr_data[[study_col]])))

    if (!"Study_label" %in% colnames(bibliography)) {
        add("bibliography_schema", FALSE, "data_source.csv has no Study_label column")
        out <- do.call(rbind, rows)
        rownames(out) <- NULL
        return(out)
    }
    bib_studies <- as.character(bibliography$Study_label)

    dupes <- unique(bib_studies[duplicated(bib_studies)])
    add(
        "bibliography_no_duplicates",
        length(dupes) == 0,
        if (length(dupes) == 0) {
            "each study appears exactly once in the bibliography"
        } else {
            sprintf("duplicate bibliography study labels: %s", paste(dupes, collapse = ", "))
        }
    )
    missing_from_bib <- setdiff(atlas_studies, bib_studies)
    add(
        "bibliography_covers_atlas",
        length(missing_from_bib) == 0,
        if (length(missing_from_bib) == 0) {
            sprintf("all %d atlas studies have a bibliography record", length(atlas_studies))
        } else {
            sprintf("atlas studies with no bibliography record: %s",
                paste(missing_from_bib, collapse = ", "))
        }
    )
    extra_in_bib <- setdiff(bib_studies, atlas_studies)
    add(
        "bibliography_no_extra_studies",
        length(extra_in_bib) == 0,
        if (length(extra_in_bib) == 0) {
            "bibliography contains no study absent from the atlas"
        } else {
            sprintf("bibliography records with no atlas cells: %s",
                paste(extra_in_bib, collapse = ", "))
        }
    )

    # Cell counts, where the bibliography states them.
    if ("Number_of_cells" %in% colnames(bibliography)) {
        atlas_counts <- table(as.character(mr_data[[study_col]]))
        mism <- character(0)
        for (i in seq_len(nrow(bibliography))) {
            s <- bib_studies[i]
            if (!s %in% names(atlas_counts)) next
            if (as.numeric(bibliography$Number_of_cells[i]) != as.numeric(atlas_counts[[s]])) {
                mism <- c(mism, sprintf(
                    "%s: bibliography %s vs atlas %s",
                    s, bibliography$Number_of_cells[i], atlas_counts[[s]]
                ))
            }
        }
        add(
            "bibliography_cell_counts",
            length(mism) == 0,
            if (length(mism) == 0) "per-study cell counts agree" else paste(mism, collapse = "; ")
        )
    }

    # PMID consistency, where the bibliography carries a PMID column.
    pmid_col <- intersect(c("Study_pmid", "PMID", "study_pmid", "Pmid"), colnames(bibliography))
    if (length(pmid_col) > 0) {
        pmid_col <- pmid_col[1]
        atlas_pmid <- unique(mr_data[, c(study_col, get_data_column("study_pmid"))])
        mism <- character(0)
        for (i in seq_len(nrow(bibliography))) {
            s <- bib_studies[i]
            hit <- atlas_pmid[atlas_pmid[[study_col]] == s, , drop = FALSE]
            if (nrow(hit) == 0) next
            if (!identical(
                as.character(bibliography[[pmid_col]][i]),
                as.character(hit[[get_data_column("study_pmid")]][1])
            )) {
                mism <- c(mism, s)
            }
        }
        add(
            "bibliography_pmid_consistency",
            length(mism) == 0,
            if (length(mism) == 0) "PMIDs agree" else paste("PMID mismatch:", paste(mism, collapse = ", "))
        )
    } else {
        add(
            "bibliography_pmid_consistency",
            FALSE,
            paste(
                "data_source.csv carries no PMID column, so study_label is the",
                "only available join key; PMID consistency cannot be checked",
                "from this export (non-blocking, documented limitation)"
            ),
            warn_only = TRUE
        )
    }

    # One PMID per study inside the atlas itself.
    per_study_pmids <- tapply(
        as.character(mr_data[[get_data_column("study_pmid")]]),
        as.character(mr_data[[study_col]]),
        function(x) length(unique(x))
    )
    multi <- names(per_study_pmids)[per_study_pmids != 1]
    add(
        "atlas_one_pmid_per_study",
        length(multi) == 0,
        if (length(multi) == 0) {
            "every study carries exactly one PMID in the atlas metadata"
        } else {
            sprintf("studies with more than one PMID: %s", paste(multi, collapse = ", "))
        }
    )

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

#' Run the contract validation and stop the app if anything failed.
#'
#' @return the contract result data.frame (invisibly) on success.
enforce_app_data_contract <- function(mr_data, expr_df = NULL, bibliography = NULL) {
    result <- validate_app_data_contract(mr_data, expr_df, bibliography)
    failures <- result[result$status == "FAIL", , drop = FALSE]
    warnings_ <- result[result$status == "WARN", , drop = FALSE]

    if (nrow(warnings_) > 0) {
        message(sprintf(
            "PlaViSca data contract - %d warning(s):\n  - %s",
            nrow(warnings_),
            paste(sprintf("%s: %s", warnings_$check, warnings_$detail), collapse = "\n  - ")
        ))
    }
    if (nrow(failures) > 0) {
        stop(sprintf(
            paste0(
                "PlaViSca startup data-contract validation FAILED (%d check(s)).\n",
                "The data in '%s' is not compatible with this version of the app.\n",
                "  - %s"
            ),
            nrow(failures), app_data_dir(),
            paste(sprintf("%s: %s", failures$check, failures$detail), collapse = "\n  - ")
        ), call. = FALSE)
    }
    message(sprintf(
        "PlaViSca data contract: %d/%d checks passed against '%s'.",
        sum(result$status == "PASS"), nrow(result), app_data_dir()
    ))
    invisible(result)
}
