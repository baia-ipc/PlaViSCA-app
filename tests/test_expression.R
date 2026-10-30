# The expression matrices and the functions reading them
# (functions/data/expression.R), and the gene menus.

test_that_("the columns of the expression matrix are gene identifiers", {
    n <- app_data("normalized_expression")
    ids <- expression_gene_ids(n)
    expect_true_(length(ids) > 1000, sprintf("expected >1000 genes, found %d", length(ids)))
    expect_true_(all(grepl(GENE_ID_PATTERN, ids)), "every gene column must carry a PVP01 identifier")
    expect_empty_(intersect(ids, colnames(app_data("mr_data"))), "metadata columns among the genes")
})

test_that_("raw and normalized lookup works for a known gene", {
    n <- app_data("normalized_expression")
    r <- app_data("raw_expression")
    m <- app_data("mr_data")
    # pick a gene with signal so the test is meaningful
    detected <- Matrix::colSums(r[, utils::head(expression_gene_ids(r), 300)] > 0)
    probe <- names(detected)[detected > 100][1]
    if (is.na(probe)) skip_test("no sufficiently expressed gene found in the first 300")
    expect_true_(probe %in% colnames(n), sprintf("gene %s must be present in both matrices", probe))
    expect_equal_(length(expression_vector(n, probe)), nrow(m), "normalized gene vector length")
    expect_equal_(length(expression_vector(r, probe)), nrow(m), "raw gene vector length")
    expect_true_(all(expression_vector(r, probe) >= 0), "raw counts must be non-negative")
    expect_true_(sum(expression_vector(n, probe) > 0) > 0, "normalized values must show signal for this gene")
})

test_that_("row-index filtering preserves cell-key alignment", {
    m <- app_data("mr_data")
    n <- app_data("normalized_expression")
    study_col <- get_data_column("study_label")
    idx <- which(m[[study_col]] == "Sa2020")
    expect_true_(length(idx) > 0, "Sa2020 cells must exist in the metadata")
    # the key invariant the gene-expression path relies on: the rows of the
    # matrix selected by row_idx are the cells of the metadata rows row_idx
    expect_equal_(rownames(n)[idx], as.character(m[[get_data_column("barcode")]][idx]),
        "barcode column and matrix rownames must stay aligned after subsetting")
    gene <- expression_gene_ids(n)[1]
    expect_equal_(expression_vector(n, gene, idx), expression_vector(n, gene)[idx],
        "the expression of the selected rows")
})

test_that_("expression_values gives a dense cells x genes matrix", {
    if (!exists("expression_values")) skip_test("expression functions not loaded")
    sparse <- Matrix::Matrix(cbind(PVP01_A = c(1, 0, 3, 0), PVP01_B = c(0, 5, 0, 6), PVP01_C = c(0, 0, 0, 7)),
                             sparse = TRUE)
    rownames(sparse) <- letters[1:4]
    values <- expression_values(sparse, c("PVP01_B", "PVP01_A"), c(2, 4))
    expect_true_(is.matrix(values) && is.numeric(values) && !inherits(values, "Matrix"), "a dense numeric matrix")
    expect_equal_(dim(values), c(2L, 2L), "one row per selected cell, one column per gene")
    expect_equal_(colnames(values), c("PVP01_B", "PVP01_A"), "genes in the order asked")
    expect_equal_(unname(values[, "PVP01_B"]), c(5, 6), "values of the selected cells")
    expect_equal_(expression_vector(sparse, "PVP01_A"), c(1, 0, 3, 0), "one gene, all cells, as a vector")
    expressed <- cell_expressed_genes(sparse, 4)
    expect_equal_(expressed$gene_id, c("PVP01_C", "PVP01_B"), "the expressed genes of a cell, most expressed first")
    expect_equal_(expressed$exp_value, c(7, 6), "their values")
    expect_equal_(nrow(cell_expressed_genes(sparse, 2)), 1L, "a cell expressing one gene")
})

test_that_("variable-gene and PCA tables cover all six studies", {
    p <- app_data_path("cleaned_dataset.rds")
    if (!file.exists(p)) skip_test("no data")
    cd <- readRDS(p)
    for (nm in c("hiv_data", "pca_df")) {
        expect_true_(!is.null(cd[[nm]]), sprintf("%s must be present in cleaned_dataset.rds", nm))
        expect_setequal_(
            unique(as.character(cd[[nm]]$study)), app_expected_studies(),
            sprintf("%s study coverage", nm)
        )
    }
})

test_that_("switching the gene menu between names and IDs keeps the selection", {
    if (!exists("convert_gene_selection")) skip_test("gene helpers not loaded")
    genes_df <- data.frame(
        gene = c("PVP01_0835600", "PVP01_0602100", "PVP01_0602200", "PVP01_0000100"),
        name = c("CSP", "ETRAMP (PVP01_0602100)", "ETRAMP (PVP01_0602200)", "PVP01_0000100")
    )
    expect_equal_(convert_gene_selection(c("CSP", "ETRAMP (PVP01_0602100)"), genes_df, TRUE),
        c("PVP01_0835600", "PVP01_0602100"), "names to IDs, shared names included")
    expect_equal_(convert_gene_selection(c("PVP01_0835600", "PVP01_0602200", "PVP01_0000100"), genes_df, FALSE),
        c("CSP", "ETRAMP (PVP01_0602200)", "PVP01_0000100"), "IDs to names; a gene without a name keeps its ID")
    expect_equal_(convert_gene_selection(c("CSP", "PVP01_0000100"), genes_df, FALSE), c("CSP", "PVP01_0000100"),
        "values already in the target form are kept")
    expect_equal_(convert_gene_selection(c("unknown", ""), genes_df, TRUE), character(0), "unknown genes dropped")
    expect_equal_(convert_gene_selection(NULL, genes_df, TRUE), character(0), "empty selection")
})
