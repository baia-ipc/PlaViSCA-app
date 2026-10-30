# The data in PLAVISCA_DATA_DIR load and match what the app expects.

test_that_("the data directory resolves from PLAVISCA_DATA_DIR", {
    expect_true_(nzchar(app_data_dir()), "app_data_dir() must return a directory")
    expect_true_(
        dir.exists(app_data_dir()),
        sprintf("PLAVISCA_DATA_DIR '%s' does not exist", app_data_dir())
    )
})

test_that_("all app data files are present", {
    for (f in unlist(APP_DATA_FILES)) {
        p <- app_data_path(f)
        expect_true_(file.exists(p), sprintf("missing data file: %s", p))
    }
})

test_that_("the atlas contains exactly the six expected studies", {
    m <- app_data("mr_data")
    expect_setequal_(
        unique(as.character(m[[get_data_column("study_label")]])),
        app_expected_studies(),
        "study_label set"
    )
})

test_that_("every required configured field exists in the data", {
    m <- app_data("mr_data")
    problems <- validate_data_columns(m, stop_on_error = FALSE)
    expect_empty_(problems, "configured fields that do not resolve")
})

test_that_("no configured field maps to an unsupported column", {
    bad <- Filter(function(k) get_data_column(k) %in% app_unsupported_columns(), data_field_keys())
    expect_empty_(bad, "fields pointing at unsupported columns")
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
    for (key in unique(c(filter_field_keys(), category_field_keys(), detail_table_field_keys(), plot_field_keys()))) {
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

test_that_("raw and normalized expression matrices are the cells of the metadata, in order", {
    m <- app_data("mr_data")
    n <- app_data("normalized_expression")
    r <- app_data("raw_expression")
    barcodes <- as.character(m[[get_data_column("barcode")]])
    expect_true_(inherits(n, "dgCMatrix"), "normalized expression is not a dgCMatrix")
    expect_true_(inherits(r, "dgCMatrix"), "raw expression is not a dgCMatrix")
    expect_true_(identical(rownames(n), barcodes), "normalized expression rows are not the metadata cells")
    expect_true_(identical(rownames(r), barcodes), "raw expression rows are not the metadata cells")
    expect_true_(identical(colnames(r), colnames(n)), "raw/normalized genes differ")
})

test_that_("no duplicate cell ids or gene ids in the expression matrices", {
    n <- app_data("normalized_expression")
    expect_empty_(rownames(n)[duplicated(rownames(n))], "duplicate expression rownames")
    expect_empty_(colnames(n)[duplicated(colnames(n))], "duplicate gene ids")
})

test_that_("all startup data checks pass, for the normalized and the raw matrix", {
    m <- app_data("mr_data")
    b <- app_data("bibliography")
    for (type in c("normalized_expression", "raw_expression")) {
        res <- check_app_data(m, app_data(type), b, expr_label = type)
        failures <- res[res$status == "FAIL", , drop = FALSE]
        expect_empty_(
            sprintf("%s: %s", failures$check, failures$detail),
            sprintf("failed startup data checks (%s)", type)
        )
    }
})

test_that_("the expression checks report a matrix that does not match the metadata", {
    m <- app_data("mr_data")
    n <- app_data("normalized_expression")
    shuffled <- n[c(2, 1, seq_len(nrow(n))[-(1:2)]), ]
    res <- check_expression_matrix(m, shuffled)
    expect_true_("FAIL" %in% res$status[res$check == "normalized_expression_cell_keys"],
        "a reordered matrix must fail the cell check")
    dense <- as.matrix(n[1:5, 1:5])
    res <- check_expression_matrix(m, dense)
    expect_true_(all(c("FAIL") %in% res$status[grepl("_class$|_row_count$|_gene_identification$", res$check)]),
        "a dense 5 x 5 matrix must fail the class, row count and gene checks")
})

test_that_("required embedding/reduction columns exist", {
    m <- app_data("mr_data")
    for (prefix in c("umap_u_", "umap_i_", "pca_u_", "pca_i_", "t_sne_")) {
        found <- grep(paste0("^", prefix, "\\d+$"), colnames(m), value = TRUE)
        expect_true_(length(found) >= 2, sprintf("expected >=2 %s* columns, found %d", prefix, length(found)))
    }
})
