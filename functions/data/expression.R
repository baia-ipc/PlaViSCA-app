# Reading gene expression from the expression matrices.
# =============================================================================
# The expression of each data type (normalized, raw) is a sparse matrix
# (Matrix::dgCMatrix), cells x genes: rownames are the cell barcodes, in the
# order of the metadata rows (mr_data), colnames the gene IDs. This is the
# only file that reads values out of it; the rest of the app works with the
# metadata and asks here for the expression of a few genes or of one cell.
# =============================================================================

loadNamespace("Matrix")

# Gene IDs look like this (PlasmoDB P01 identifiers).
GENE_ID_PATTERN <- "^PVP01_"

# The gene IDs of an expression matrix.
expression_gene_ids <- function(expression) {
    colnames(expression)
}

# Expression of the genes `gene_ids` in the cells `row_idx` (all cells if
# NULL): a dense numeric matrix, cells x genes, with the gene IDs as column
# names and no row names. Only the asked columns are made dense: their
# nonzero entries are read straight from the column slots of the sparse
# matrix (for any other kind of matrix, by subsetting it).
expression_values <- function(expression, gene_ids, row_idx = NULL) {
    if (!inherits(expression, "dgCMatrix")) {
        values <- as.matrix(expression[, gene_ids, drop = FALSE])
    } else {
        columns <- match(gene_ids, colnames(expression))
        if (anyNA(columns)) {
            stop(sprintf("genes not in the expression matrix: %s",
                         paste(gene_ids[is.na(columns)], collapse = ", ")))
        }
        values <- matrix(0, nrow = nrow(expression), ncol = length(columns))
        starts <- expression@p
        for (k in seq_along(columns)) {
            entries <- seq.int(starts[columns[k]] + 1, length.out = starts[columns[k] + 1] - starts[columns[k]])
            values[expression@i[entries] + 1, k] <- expression@x[entries]
        }
    }
    if (!is.null(row_idx)) {
        values <- values[row_idx, , drop = FALSE]
    }
    storage.mode(values) <- "double"
    dimnames(values) <- list(NULL, gene_ids)
    values
}

# Expression of one gene in the cells `row_idx` (all cells if NULL), as a
# numeric vector.
expression_vector <- function(expression, gene_id, row_idx = NULL) {
    expression_values(expression, gene_id, row_idx)[, 1]
}

# The genes expressed (value > 0) in the cell at row `row`: a data.frame
# with gene_id and exp_value, most expressed first. One row of a
# column-compressed matrix is found by scanning every column (about 0.3 s
# for the atlas), so this is for the details of one cell, not for plots.
cell_expressed_genes <- function(expression, row) {
    values <- as.numeric(expression[row, ])
    expressed <- which(values > 0)
    out <- data.frame(
        gene_id = colnames(expression)[expressed],
        exp_value = values[expressed],
        stringsAsFactors = FALSE
    )
    out[order(-out$exp_value), , drop = FALSE]
}
