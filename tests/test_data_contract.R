# Data-contract tests: the candidate atlas loads and matches what the app expects.

test_that_("candidate data directory resolves from PLAVISCA_DATA_DIR", {
    expect_true_(nzchar(app_data_dir()), "app_data_dir() must return a directory")
    expect_true_(
        dir.exists(app_data_dir()),
        sprintf("PLAVISCA_DATA_DIR '%s' does not exist", app_data_dir())
    )
})

test_that_("all four app data files are present", {
    for (f in unlist(APP_DATA_FILES)) {
        p <- app_data_path(f)
        expect_true_(file.exists(p), sprintf("missing data file: %s", p))
    }
})

test_that_("cleaned_dataset.rds loads and has the expected cell count", {
    m <- app_data("mr_data")
    expect_equal_(nrow(m), app_expected_cell_count(), "metadata row count")
})

test_that_("the atlas contains exactly the six expected studies", {
    m <- app_data("mr_data")
    expect_setequal_(
        unique(as.character(m[[get_data_column("study_label")]])),
        app_expected_studies(),
        "study_label set"
    )
})

test_that_("every required configured field exists in the candidate data", {
    m <- app_data("mr_data")
    problems <- validate_data_columns(m, stop_on_error = FALSE)
    expect_empty_(problems, "configured fields that do not resolve")
})

test_that_("no configured field maps to an obsolete pre-Phase-2 column", {
    bad <- Filter(function(k) get_data_column(k) %in% app_obsolete_columns(), data_field_keys())
    expect_empty_(bad, "fields still pointing at obsolete columns")
})

test_that_("cell identifiers are unique and match the metadata rownames", {
    m <- app_data("mr_data")
    bc <- as.character(m[[get_data_column("barcode")]])
    expect_empty_(bc[duplicated(bc)], "duplicate cell ids")
    expect_true_(identical(rownames(m), bc), "rownames must equal the barcode column")
})

test_that_("no literal 'NA'/'None' strings in app-visible categorical fields", {
    m <- app_data("mr_data")
    offenders <- character(0)
    for (key in unique(c(filter_field_keys(), category_field_keys(), detail_table_field_keys()))) {
        col <- get_data_column(key)
        if (!col %in% colnames(m)) next
        v <- m[[col]]
        if (!(is.character(v) || is.factor(v))) next
        chr <- as.character(v)
        n <- sum(!is.na(chr) & chr %in% LITERAL_MISSING_TOKENS)
        if (n > 0) offenders <- c(offenders, sprintf("%s (%d)", col, n))
    }
    expect_empty_(offenders, "literal missing-ish strings in app-visible fields")
})

test_that_("raw, normalized and metadata tables describe the same cells", {
    m <- app_data("mr_data")
    n <- app_data("norm_df")
    r <- app_data("raw_df")
    expect_equal_(nrow(n), nrow(m), "normalize_df row count")
    expect_equal_(nrow(r), nrow(m), "raw_df row count")
    expect_true_(setequal(rownames(n), rownames(m)), "normalize_df/metadata cell sets differ")
    expect_true_(identical(rownames(r), rownames(n)), "raw_df/normalize_df cell order differs")
    expect_true_(identical(colnames(r), colnames(n)), "raw_df/normalize_df columns differ")
})

test_that_("no duplicate cell ids in the expression tables", {
    n <- app_data("norm_df")
    expect_empty_(rownames(n)[duplicated(rownames(n))], "duplicate expression rownames")
})

test_that_("full startup contract validation passes with no FAILs", {
    m <- app_data("mr_data")
    n <- app_data("norm_df")
    b <- app_data("bibliography")
    res <- validate_app_data_contract(m, n, b)
    failures <- res[res$status == "FAIL", , drop = FALSE]
    expect_empty_(
        sprintf("%s: %s", failures$check, failures$detail),
        "startup contract failures"
    )
})

test_that_("required embedding/reduction columns exist", {
    m <- app_data("mr_data")
    for (prefix in c("umap_u_", "umap_i_", "pca_u_", "pca_i_", "t_sne_")) {
        found <- grep(paste0("^", prefix, "\\d+$"), colnames(m), value = TRUE)
        expect_true_(length(found) >= 2, sprintf("expected >=2 %s* columns, found %d", prefix, length(found)))
    }
})
