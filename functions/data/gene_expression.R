# Data for the gene expression plots.
# =============================================================================
# Builds the small per-plot tables (grouping column + expression of the
# selected genes) used by the Gene expression tab.
# =============================================================================

# Table of the grouping column (and `extra_cols`) of the cells in `meta` and,
# in `gene_expr`, the expression of the genes `gene_exp` (IDs; the mean when
# there are several), taken from the expression matrix `expression` at the
# rows attr(meta, "row_idx") (the first rows of the matrix if there is no
# such attribute, which is only right for the unfiltered metadata).
build_plot_slim <- function(meta, expression, gene_exp, group_col, extra_cols = NULL) {
    if (is.null(meta)) meta <- data.frame()
    if (!(group_col %in% colnames(meta))) {
        meta[[group_col]] <- NA_character_
    }
    slim <- meta %>% select(any_of(unique(c(group_col, extra_cols))))
    row_idx <- attr(meta, "row_idx")
    if (is.null(row_idx)) row_idx <- seq_len(nrow(slim))

    found <- gene_exp[!is.na(gene_exp) & gene_exp %in% colnames(expression)]
    if (is.null(expression) || length(found) == 0) {
        slim$gene_expr <- rep(NA_real_, nrow(slim))
        return(slim)
    }
    values <- expression_values(expression, found, row_idx)
    if (nrow(values) != nrow(slim)) {
        stop(sprintf("%d cells in the metadata but %d rows of expression", nrow(slim), nrow(values)))
    }
    slim$gene_expr <- if (ncol(values) == 1) values[, 1] else rowMeans(values, na.rm = TRUE)
    slim
}

# Sheets of the "Data" download of a gene expression card: the expression of
# the genes `genes` (names, or IDs if `check` is TRUE) in the cells `cells`
# (list(data, genes, expression), as in the Gene expression tab) with the grouping
# column `group`, and the settings of the download. `filter_values` gives the
# filter selections, named by filter label. Stops with a message when there is
# nothing to download.
gene_expression_sheets <- function(cells, genes, group, check, data_type, filter_values) {
    if (length(genes) == 0 || is.null(group)) {
        stop("Please select a gene and grouping option")
    }
    ids <- if (isTRUE(check)) {
        genes
    } else {
        sapply(genes, function(gene) get_gene_id(gene, cells$genes), USE.NAMES = FALSE)
    }
    ok <- !is.na(ids) & ids != ""
    if (!any(ok)) {
        stop("No valid genes found for the current selection")
    }

    plot_data <- build_plot_slim(cells$data, cells$expression, ids[ok], group)
    if (is.null(plot_data) || nrow(plot_data) == 0) {
        stop("No data available for the selected gene and filters")
    }

    metadata <- data.frame(
        Parameter = c(
            "Analysis Type", "Data Type", "Gene ID(s)", "Gene Name(s)", "Group By",
            names(filter_values), "Download Date"
        ),
        Value = c(
            if (sum(ok) == 1) "Single Gene" else paste("Average of", sum(ok), "genes"),
            data_type,
            paste(ids[ok], collapse = ", "),
            paste(genes[ok], collapse = ", "),
            as.character(group),
            unname(filter_values),
            as.character(Sys.time())
        ),
        stringsAsFactors = FALSE
    )
    list("Expression Data" = plot_data, "Metadata" = metadata)
}

# File name of the "Data" download of a gene expression card.
gene_expression_file_name <- function(genes) {
    if (length(genes) == 0) genes <- "gene"
    if (length(genes) > 1) {
        paste0("gene_expression_", length(genes), "_genes_", Sys.Date(), ".xlsx")
    } else {
        paste0("gene_expression_", genes[1], "_", Sys.Date(), ".xlsx")
    }
}
