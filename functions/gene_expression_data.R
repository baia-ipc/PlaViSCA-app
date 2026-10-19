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
