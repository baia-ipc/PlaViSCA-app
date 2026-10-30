# Startup checks: does the data match what the app expects?
# =============================================================================
# The app stops at startup, with a message saying what is wrong, when the data
# in PLAVISCA_DATA_DIR does not match config/data_columns.yml (cell count,
# studies, columns, life-cycle stages, expression matrix, bibliography).
# Starting anyway would show empty dropdowns or misleading plots.
#
# The checks do not depend on Shiny, so the tests can call them directly.
# =============================================================================

.check_row <- function(check, status, detail) {
    data.frame(check = check, status = status, detail = detail, stringsAsFactors = FALSE)
}

#' Check the loaded data against what the app expects.
#'
#' @param mr_data     cleaned_dataset.rds$mr_data
#' @param expression  the normalized (or raw) expression matrix, cells x
#'                    genes (Matrix::dgCMatrix), or NULL to skip
#' @param bibliography data_source.csv as a data.frame, or NULL to skip
#' @param expr_label  label used in messages for `expression`
#' @return a data.frame of check/status/detail rows. Status is "PASS", "FAIL"
#'         or "WARN".
check_app_data <- function(mr_data,
                           expression = NULL,
                           bibliography = NULL,
                           expr_label = "normalized_expression") {
    rows <- list()
    add <- function(check, ok, detail, warn_only = FALSE) {
        status <- if (ok) "PASS" else if (warn_only) "WARN" else "FAIL"
        rows[[length(rows) + 1]] <<- .check_row(check, status, detail)
    }

    cols <- colnames(mr_data)

    # --- 1. every required configured field resolves --------------------------
    # This runs before any check that reads a configured column, so data
    # with different columns gives a clear message rather than an obscure
    # subsetting error further down.
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
            "columns_match",
            FALSE,
            paste(
                "the loaded data does not have the columns listed in",
                "config/data_columns.yml, so the remaining checks were skipped.",
                "Point PLAVISCA_DATA_DIR at the app_data directory written by",
                "the preprocessing pipeline."
            )
        )
        out <- do.call(rbind, rows)
        rownames(out) <- NULL
        return(out)
    }

    # --- 2. studies -----------------------------------------------------------
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

    # --- 3. no configured field maps to an unsupported column -----------------
    unsupported <- app_unsupported_columns()
    unsupported_targets <- Filter(
        function(k) get_data_column(k) %in% unsupported,
        data_field_keys()
    )
    add(
        "no_field_uses_unsupported_column",
        length(unsupported_targets) == 0,
        if (length(unsupported_targets) == 0) {
            "no configured field points at an unsupported column"
        } else {
            paste(unsupported_targets, collapse = ", ")
        }
    )

    # --- 4. unsupported columns are not required ------------------------------
    present_unsupported <- intersect(unsupported, cols)
    add(
        "unsupported_columns_not_required",
        length(intersect(present_unsupported, required_data_columns())) == 0,
        sprintf(
            "unsupported columns present in the data, not used by the app: [%s]",
            paste(present_unsupported, collapse = ", ")
        )
    )

    # --- 5. cell identifiers --------------------------------------------------
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

    # --- 6. no literal "NA" categories in app-visible categorical fields ------
    visible_keys <- unique(c(filter_field_keys(), category_field_keys(), plot_field_keys()))
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

    # --- 7. broad / detailed stage compatibility ------------------------------
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

    # --- 8. reference time point fields ---------------------------------------
    idc_col <- get_data_column("idc_reference_class")
    add(
        "idc_fields_present",
        all(c(idc_col, get_data_column("idc_heuristic_stage_bin"),
            get_data_column("idc_similarity_score_delta")) %in% cols),
        "reference time point, its stage and the SingleR delta are present"
    )
    add(
        "hour_post_invasion_not_required",
        !("hour_post_invasion" %in% required_data_columns()),
        "the app must not require the hour_post_invasion column"
    )
    # IDC classification must only exist for blood-eligible cells
    tissue_col <- get_data_column("tissue_or_sample_type")
    leaked <- sum(!is.na(mr_data[[idc_col]]) & mr_data[[tissue_col]] != "Host blood")
    add(
        "idc_blood_only",
        leaked == 0,
        sprintf("%d non-blood cells carry an IDC reference-time class", leaked)
    )

    # --- 9. gametocyte provenance not collapsed -------------------------------
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
        sprintf("%d non-blood cells carry a PlaViSCA-inferred gametocyte sex", gam_leak)
    )

    # --- 10. embeddings -------------------------------------------------------
    for (prefix in c("umap_u_", "umap_i_", "pca_u_", "pca_i_", "t_sne_")) {
        found <- grep(paste0("^", prefix, "\\d+$"), cols, value = TRUE)
        add(
            paste0("embedding_", sub("_$", "", prefix)),
            length(found) >= 2,
            sprintf("found %d %s* columns", length(found), prefix)
        )
    }

    # --- 11. expression matrix: the cells of the metadata, genes as columns --
    if (!is.null(expression)) {
        expr <- check_expression_matrix(mr_data, expression, expr_label)
        rows <- c(rows, split(expr, seq_len(nrow(expr))))
    }

    # --- 12. bibliography -----------------------------------------------------
    if (!is.null(bibliography)) {
        bib <- validate_bibliography(mr_data, bibliography)
        rows <- c(rows, split(bib, seq_len(nrow(bib))))
    }

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

#' Check an expression matrix against the metadata: a sparse matrix
#' (Matrix::dgCMatrix) whose rows are the cells of the metadata, in the same
#' order, and whose columns are gene IDs.
#'
#' @return a data.frame of check/status/detail rows, named by `label`.
check_expression_matrix <- function(mr_data, expression, label = "normalized_expression") {
    checks <- expression_checks(mr_data, expression)
    out <- do.call(rbind, lapply(names(checks), function(name) {
        .check_row(paste0(label, "_", name), if (isTRUE(checks[[name]]$ok)) "PASS" else "FAIL",
                   checks[[name]]$detail)
    }))
    rownames(out) <- NULL
    out
}

# The checks of check_expression_matrix(): a list of list(ok, detail), by
# check name.
expression_checks <- function(mr_data, expression) {
    barcodes <- as.character(mr_data[[get_data_column("barcode")]])
    # an object without dimensions (not a matrix at all) fails every check
    # with a message, rather than stopping the checks with an error
    n_rows <- if (is.null(dim(expression))) 0L else nrow(expression)
    gene_ids <- colnames(expression)
    not_genes <- gene_ids[!grepl(GENE_ID_PATTERN, gene_ids)]
    list(
        class = list(
            ok = inherits(expression, "dgCMatrix"),
            detail = sprintf("expected a Matrix::dgCMatrix, found %s", paste(class(expression), collapse = "/"))
        ),
        row_count = list(
            ok = n_rows == nrow(mr_data),
            detail = sprintf("%d expression rows vs %d metadata rows", n_rows, nrow(mr_data))
        ),
        cell_keys = list(
            ok = identical(rownames(expression), barcodes),
            detail = "expression rows must be the cells of the metadata, in the same order"
        ),
        no_duplicate_cells = list(
            ok = !any(duplicated(rownames(expression))),
            detail = sprintf("%d duplicate expression rownames", sum(duplicated(rownames(expression))))
        ),
        gene_identification = list(
            ok = length(gene_ids) > 1000 && length(not_genes) == 0 && !any(duplicated(gene_ids)),
            detail = if (length(not_genes) == 0) {
                sprintf("%d gene columns, every one a PVP01 identifier", length(gene_ids))
            } else {
                sprintf("%d columns are not gene identifiers: %s", length(not_genes),
                        paste(utils::head(not_genes, 5), collapse = ", "))
            }
        )
    )
}

#' Check that the bibliography (data_source.csv) lists exactly the studies in
#' the data, with matching cell counts and PMIDs.
#'
#' The bibliography is a separate table from the cell metadata; without this
#' check the two could disagree unnoticed.
validate_bibliography <- function(mr_data, bibliography) {
    rows <- list()
    add <- function(check, ok, detail, warn_only = FALSE) {
        status <- if (ok) "PASS" else if (warn_only) "WARN" else "FAIL"
        rows[[length(rows) + 1]] <<- .check_row(check, status, detail)
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
                "from these data (warning only)"
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

#' Run check_app_data() and stop the app if any check failed.
#'
#' @return the check results as a data.frame (invisibly) when all pass.
stop_if_app_data_invalid <- function(mr_data, expression = NULL, bibliography = NULL,
                                     expr_label = "normalized_expression") {
    result <- check_app_data(mr_data, expression, bibliography, expr_label)
    failures <- result[result$status == "FAIL", , drop = FALSE]
    warnings_ <- result[result$status == "WARN", , drop = FALSE]

    if (nrow(warnings_) > 0) {
        message(sprintf(
            "PlaViSCA data checks: %d warning(s):\n  - %s",
            nrow(warnings_),
            paste(sprintf("%s: %s", warnings_$check, warnings_$detail), collapse = "\n  - ")
        ))
    }
    if (nrow(failures) > 0) {
        stop(sprintf(
            paste0(
                "PlaViSCA cannot start: %d data check(s) failed.\n",
                "The data in '%s' do not match config/data_columns.yml:\n",
                "  - %s"
            ),
            nrow(failures), app_data_dir(),
            paste(sprintf("%s: %s", failures$check, failures$detail), collapse = "\n  - ")
        ), call. = FALSE)
    }
    message(sprintf(
        "PlaViSCA data checks: %d/%d passed for '%s'.",
        sum(result$status == "PASS"), nrow(result), app_data_dir()
    ))
    invisible(result)
}
