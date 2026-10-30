# The expressed genes of one cell, shown when a cell of an embedding plot is
# opened (Embeddings tab).

# Row of the cell `cell_key` (the key of a point of an embedding plot: its
# position in `cells`, or its barcode) in the expression matrix `expression`,
# whose rownames are the barcodes. `cells` are the plotted cells, with attr
# "row_idx" giving their rows in the matrix. NA when the cell is not found.
cell_row <- function(cell_key, cells, expression) {
    row_idx <- attr(cells, "row_idx")
    position <- suppressWarnings(as.integer(as.character(cell_key)))
    if (!is.na(position) && !is.null(cells) && position >= 1 && position <= nrow(cells)) {
        if (length(row_idx) >= position) return(row_idx[position])
    } else if (!is.null(cells) && any(cell_key == cells$barcode, na.rm = TRUE)) {
        in_cells <- which(cells$barcode == cell_key)[1]
        if (length(row_idx) >= in_cells) return(row_idx[in_cells])
    } else if (cell_key %in% rownames(expression)) {
        return(match(cell_key, rownames(expression)))
    }
    NA_integer_
}

# Genes expressed in row `row` of the expression matrix `expression`
# (expression > 0), with their names and descriptions from `gene_annotation`
# (columns id, name, description), most expressed first.
cell_expression_table <- function(row, expression, gene_annotation) {
    empty <- tibble::tibble(gene_id = character(), gene_name = character(),
                            exp_value = numeric(), description = character())
    if (is.na(row) || is.null(expression)) {
        return(empty)
    }
    cell_expressed_genes(expression, row) %>%
        left_join(gene_annotation, by = c("gene_id" = "id")) %>%
        select(gene_id, gene_name = name, exp_value, description) %>%
        tibble::as_tibble()
}
