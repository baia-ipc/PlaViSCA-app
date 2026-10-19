# The expressed genes of one cell, shown when a cell of an embedding plot is
# opened (Embeddings tab).

# Row of the cell `cell_key` (the key of a point of an embedding plot: its
# position in `cells`, or its barcode) in `expression_table`. `cells` are the
# plotted cells, with attr "row_idx" giving their rows in `expression_table`.
# NA when the cell is not found.
cell_row <- function(cell_key, cells, expression_table) {
    row_idx <- attr(cells, "row_idx")
    position <- suppressWarnings(as.integer(as.character(cell_key)))
    if (!is.na(position) && !is.null(cells) && position >= 1 && position <= nrow(cells)) {
        if (length(row_idx) >= position) return(row_idx[position])
    } else if (!is.null(cells) && any(cell_key == cells$barcode, na.rm = TRUE)) {
        in_cells <- which(cells$barcode == cell_key)[1]
        if (length(row_idx) >= in_cells) return(row_idx[in_cells])
    } else if (any(cell_key == expression_table$barcode, na.rm = TRUE)) {
        return(which(expression_table$barcode == cell_key)[1])
    }
    NA_integer_
}

# Genes expressed in row `row` of `expression_table` (expression > 0), with
# their names and descriptions from `gene_annotation` (columns id, name,
# description), most expressed first.
cell_expression_table <- function(row, expression_table, gene_annotation) {
    empty <- tibble::tibble(gene_id = character(), gene_name = character(),
                            exp_value = numeric(), description = character())
    gene_cols <- grep("^PVP01", colnames(expression_table), value = TRUE)
    if (is.na(row) || length(gene_cols) == 0) {
        return(empty)
    }
    tibble::tibble(
        gene_id = gene_cols,
        exp_value = unlist(as.data.frame(expression_table)[row, gene_cols], use.names = FALSE)
    ) %>%
        filter(exp_value > 0) %>%
        left_join(gene_annotation, by = c("gene_id" = "id")) %>%
        arrange(desc(exp_value)) %>%
        select(gene_id, gene_name = name, exp_value, description)
}
