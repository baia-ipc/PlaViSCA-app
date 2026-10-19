# Data for the gene expression plots.
# =============================================================================
# Builds the small per-plot tables (grouping column + expression of the
# selected genes) used by the Gene expression tab.
# =============================================================================

# Table of the grouping column (and `extra_cols`) of the cells in `meta` and
# the expression of the genes `gene_exp` (IDs), taken from `full_mat` at the
# rows attr(meta, "row_idx").
build_plot_slim <- function(meta, full_mat, gene_exp, group_col, extra_cols = NULL) {
    if (is.null(meta)) meta <- data.frame()

    # Ensure the group column exists in the returned slim df
    needed <- unique(c(group_col, extra_cols))
    available <- intersect(needed, colnames(meta))
    if (!(group_col %in% colnames(meta))) {
        meta[[group_col]] <- NA_character_
    }

    slim <- meta %>% select(any_of(needed))

    # Extract gene expression vector from the full matrix using row_idx
    row_idx <- attr(meta, "row_idx")

    # Handle multiple genes (for averaging)
    if (length(gene_exp) > 1) {
        # Multiple genes - calculate average
        gene_matrices <- list()

        for (gene_id in gene_exp) {
            if (!is.null(full_mat) && gene_id %in% colnames(full_mat)) {
                if (!is.null(row_idx) && length(row_idx) > 0) {
                    gene_vec <- full_mat[[gene_id]][row_idx]
                } else {
                    gene_vec <- full_mat[[gene_id]]
                    if (nrow(slim) > 0) gene_vec <- gene_vec[seq_len(nrow(slim))]
                }
                gene_matrices[[gene_id]] <- gene_vec
            }
        }

        # Calculate average if we have any valid genes
        if (length(gene_matrices) > 0) {
            gene_matrix <- do.call(cbind, gene_matrices)
            gene_vec <- rowMeans(gene_matrix, na.rm = TRUE)
        } else {
            gene_vec <- rep(NA, nrow(slim))
        }
    } else {
        # Single gene
        if (!is.null(full_mat) && !is.null(gene_exp) && gene_exp %in% colnames(full_mat)) {
            if (!is.null(row_idx) && length(row_idx) > 0) {
                # Use row indices - this is the correct filtered subset
                gene_vec <- full_mat[[gene_exp]][row_idx]
            } else {
                # No row indices: use the first rows (only correct for unfiltered data)
                gene_vec <- full_mat[[gene_exp]]
                if (nrow(slim) > 0) gene_vec <- gene_vec[seq_len(nrow(slim))]
            }
        } else {
            gene_vec <- rep(NA, nrow(slim))
        }
    }

    # Make sure the expression vector has one value per row of the slim table
    if (length(gene_vec) != nrow(slim)) {
        if (length(gene_vec) > nrow(slim)) {
            gene_vec <- gene_vec[1:nrow(slim)]
        } else {
            gene_vec <- c(gene_vec, rep(NA, nrow(slim) - length(gene_vec)))
        }
    }

    slim$gene_expr <- gene_vec

    return(slim)
}

# Sheets of the "Data" download of a gene expression card: the expression of
# the genes `genes` (names, or IDs if `check` is TRUE) in the cells `cells`
# (list(data, genes, full), as in the Gene expression tab) with the grouping
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

    plot_data <- build_plot_slim(cells$data, cells$full, ids[ok], group)
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
