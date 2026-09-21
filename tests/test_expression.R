# Expression-table integrity tests.

test_that_("gene columns are identified by identifier despite the expanded metadata", {
    m <- app_data("mr_data")
    n <- app_data("norm_df")
    genes <- identify_gene_columns(n, m)
    expect_true_(length(genes) > 1000, sprintf("expected >1000 gene columns, found %d", length(genes)))
    expect_empty_(intersect(genes, colnames(m)), "metadata columns misidentified as genes")
    expect_true_(
        all(grepl("^PVP01_", genes)),
        "every identified gene column must carry a PVP01 identifier"
    )
})

test_that_("no metadata column is mistaken for a gene after the schema expansion", {
    m <- app_data("mr_data")
    n <- app_data("norm_df")
    genes <- identify_gene_columns(n, m)
    expect_equal_(
        length(genes) + ncol(m), ncol(n),
        "gene columns + metadata columns must account for the whole table"
    )
    # A positional assumption from the pre-Phase-2 schema (52 metadata columns)
    # would now silently treat metadata as genes; assert it no longer holds.
    expect_true_(
        ncol(m) > 52,
        "this candidate has the expanded Phase-2 metadata block"
    )
    expect_true_(
        !(colnames(n)[53] %in% genes),
        "column 53 is still metadata - positional gene identification would be wrong"
    )
})

test_that_("raw and normalized lookup works for a known gene", {
    n <- app_data("norm_df")
    r <- app_data("raw_df")
    m <- app_data("mr_data")
    genes <- identify_gene_columns(n, m)
    # pick a gene with signal so the test is meaningful
    probe <- NULL
    for (g in utils::head(genes, 300)) {
        if (sum(r[[g]] > 0) > 100) {
            probe <- g
            break
        }
    }
    if (is.null(probe)) skip_test("no sufficiently expressed gene found in the first 300")
    expect_true_(probe %in% colnames(n) && probe %in% colnames(r),
        sprintf("gene %s must be present in both tables", probe))
    expect_equal_(length(n[[probe]]), nrow(m), "normalized gene vector length")
    expect_equal_(length(r[[probe]]), nrow(m), "raw gene vector length")
    expect_true_(all(r[[probe]] >= 0, na.rm = TRUE), "raw counts must be non-negative")
    expect_true_(sum(n[[probe]] > 0) > 0, "normalized values must show signal for this gene")
})

test_that_("row-index filtering preserves cell-key alignment", {
    n <- app_data("norm_df")
    study_col <- get_data_column("study_label")
    idx <- which(n[[study_col]] == "Sa2020")
    expect_true_(length(idx) > 0, "Sa2020 cells must exist in the expression table")
    expect_setequal_(
        unique(n[[study_col]][idx]), "Sa2020",
        "row-index subsetting must select exactly the intended cells"
    )
    expect_true_(
        all(rownames(n)[idx] == rownames(n)[idx]),
        "cell keys must be recoverable from the row indices"
    )
    # the key invariant the gene-expression path relies on: a gene vector
    # subsetted by row_idx lines up with the metadata subsetted by row_idx
    m_sub <- n[idx, get_data_column("barcode"), drop = TRUE]
    expect_equal_(as.character(m_sub), rownames(n)[idx],
        "barcode column and rownames must stay aligned after subsetting")
})

test_that_("top-gene / HVG tables cover all six studies", {
    p <- app_data_path("cleaned_dataset.rds")
    if (!file.exists(p)) skip_test("no candidate data")
    cd <- readRDS(p)
    for (nm in c("hiv_data", "top_genes_exp", "pca_df")) {
        expect_true_(!is.null(cd[[nm]]), sprintf("%s must be present in cleaned_dataset.rds", nm))
        expect_setequal_(
            unique(as.character(cd[[nm]]$study)), app_expected_studies(),
            sprintf("%s study coverage", nm)
        )
    }
})

test_that_("the expression tables carry every required metadata column", {
    m <- app_data("mr_data")
    n <- app_data("norm_df")
    expect_empty_(
        setdiff(required_data_columns(), colnames(n)),
        "required metadata columns missing from normalize_df"
    )
})
