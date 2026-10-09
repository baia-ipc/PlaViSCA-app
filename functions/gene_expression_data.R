# Data for the gene expression plots.
# =============================================================================
# Builds the small per-plot tables (grouping column + expression of the
# selected genes) used by the Gene expression tab, with optional caching.
# =============================================================================

# Generate df for gene_exp0
df_exp0 <- function(gene_data, gene_input, group_input, check_exp, full_data = NULL, cache_env = NULL) {
    full_mat <- if (!is.null(full_data)) full_data else NULL
    meta_source <- if (!is.null(gene_data) && !is.null(gene_data$data)) gene_data$data else data.frame()
    row_idx <- if (!is.null(gene_data) && !is.null(gene_data$data)) attr(gene_data$data, "row_idx") else NULL
    genes <- gene_data$genes

    # CRITICAL FIX: Ensure row_idx is preserved and used correctly
    if (!is.null(row_idx) && length(row_idx) > 0) {
        if (nrow(meta_source) != length(row_idx)) {
            # Data is not pre-filtered, use row_idx to filter it
            meta_source <- meta_source[row_idx, , drop = FALSE]
            attr(meta_source, "row_idx") <- row_idx
        }
    }

    gene_exp <- if (!check_exp) {
        get_gene_id(gene_input, genes)
    } else {
        gene_input
    }

    # Return NULL if gene is invalid to prevent empty plots
    if (is.null(gene_exp) || is.na(gene_exp) || gene_exp == "") {
        return(NULL)
    }

    gene_cols <- character(0)
    if (!is.null(genes) && nrow(genes) > 0 && "gene" %in% colnames(genes)) {
        gene_cols <- genes$gene
    }

    meta_cols <- setdiff(colnames(meta_source), gene_cols)
    embed_patterns <- c("^umap", "^pca", "^t_sne", "^tsne", "^tSNE")
    embed_regex <- paste(embed_patterns, collapse = "|")
    meta_cols <- meta_cols[!grepl(embed_regex, meta_cols, ignore.case = TRUE)]

    if (!(group_input %in% meta_cols)) {
        meta_cols <- unique(c(meta_cols, group_input))
    }

    row_idx_len <- length(row_idx) %||% nrow(meta_source)
    cache_key_meta <- make_cache_key("slim_meta", row_idx_len)
    if (!is.null(cache_env) && exists(cache_key_meta, envir = cache_env, inherits = FALSE)) {
        slim_meta <- cache_env[[cache_key_meta]]
    } else {
        slim_meta <- meta_source %>% select(any_of(meta_cols))
        if (!is.null(cache_env)) {
            try(cache_env[[cache_key_meta]] <- slim_meta, silent = TRUE)
        }
    }

    # Build cache key for full slim_df and check cache first (if provided)
    cache_key <- make_cache_key("slim_df", gene_exp, group_input, row_idx_len)
    if (!is.null(cache_env) && exists(cache_key, envir = cache_env, inherits = FALSE)) {
        cached <- cache_env[[cache_key]]
        return(cached)
    }

    # Use helper to build the minimal slim dataframe for plotting
    slim_meta <- build_plot_slim(slim_meta, full_mat, gene_exp, group_input)

    # Free the potentially large full matrix reference
    if (is.null(full_data)) rm(df_full)
    invisible(gc(verbose = FALSE))

    if (!is.null(cache_env)) {
        try(cache_env[[cache_key]] <- slim_meta, silent = TRUE)
    }

    return(slim_meta)
}

# Generate df for gene_expi
df_expi <- function(id, input, gene_data, get_gene_id, full_data = NULL, cache_env = NULL) {
    reactive({
        tryCatch(
            {
                req(
                    input[[paste0("genes_exp_", id)]],
                    input[[paste0("group_exp_", id)]]
                )
                # full_data is the large expression matrix; use meta_source for metadata
                full_mat <- if (!is.null(full_data)) full_data else NULL
                meta_source <- if (!is.null(gene_data) && !is.null(gene_data$data)) gene_data$data else data.frame()
                # row indices of filtered metadata
                row_idx <- if (!is.null(gene_data) && !is.null(gene_data$data)) attr(gene_data$data, "row_idx") else NULL

                # Check_exp input
                check_exp_input <- input[[paste0("check_exp_", id)]]
                if (is.null(check_exp_input)) {
                    check_exp_input <- FALSE
                }

                gene_exp <- if (!check_exp_input) {
                    get_gene_id(input[[paste0("genes_exp_", id)]], gene_data$genes)
                } else {
                    input[[paste0("genes_exp_", id)]]
                }

                # Validate gene_exp is not NULL or empty
                if (is.null(gene_exp) || is.na(gene_exp) || gene_exp == "") {
                    return(NULL)
                }

                # Identify gene columns from the genes annotation
                gene_cols <- character(0)
                if (!is.null(gene_data$genes) && nrow(gene_data$genes) > 0 && "gene" %in% colnames(gene_data$genes)) {
                    gene_cols <- gene_data$genes$gene
                }

                meta_cols <- setdiff(colnames(meta_source), gene_cols)

                # Remove embedding coordinate columns to keep the slim dataframe small
                embed_patterns <- c("^umap", "^pca", "^t_sne", "^tsne", "^tSNE")
                embed_regex <- paste(embed_patterns, collapse = "|")
                meta_cols <- meta_cols[!grepl(embed_regex, meta_cols, ignore.case = TRUE)]

                group_col <- input[[paste0("group_exp_", id)]]

                # Ensure the grouping column is present in metadata
                if (!(group_col %in% meta_cols)) {
                    meta_cols <- unique(c(meta_cols, group_col))
                }

                # Build cache key for metadata and check cache first (if provided)
                row_idx_len <- NA
                if (!is.null(gene_data) && !is.null(gene_data$data)) row_idx_len <- length(attr(gene_data$data, "row_idx"))
                cache_key_meta <- make_cache_key("slim_meta", row_idx_len)
                if (!is.null(cache_env) && exists(cache_key_meta, envir = cache_env, inherits = FALSE)) {
                    slim_df <- cache_env[[cache_key_meta]]
                } else {
                    slim_df <- meta_source %>% select(any_of(meta_cols))
                    if (!is.null(cache_env)) {
                        try(cache_env[[cache_key_meta]] <- slim_df, silent = TRUE)
                    }
                }

                # Build cache key for full slim_df and check cache first (if provided)
                cache_key <- make_cache_key("slim_df", gene_exp, group_col, row_idx_len, id)
                if (!is.null(cache_env) && exists(cache_key, envir = cache_env, inherits = FALSE)) {
                    return(cache_env[[cache_key]])
                }

                slim_df <- build_plot_slim(slim_df, full_mat, gene_exp, group_col)
                invisible(gc(verbose = FALSE))

                if (!is.null(cache_env)) try(cache_env[[cache_key]] <- slim_df, silent = TRUE)
                return(slim_df)
            },
            error = function(e) {
                return(NULL)
            }
        )
    })
}

# Non-reactive variant of df_expi for headless testing and batch runs.
df_expi_nr <- function(id, input_list, gene_data, get_gene_id, full_data = NULL, cache_env = NULL) {
    gene_input <- input_list[[paste0("genes_exp_", id)]]
    group_input <- input_list[[paste0("group_exp_", id)]]
    if (is.null(gene_input) || is.null(group_input)) {
        return(NULL)
    }

    # Avoid subsetting the potentially huge full matrix; keep the full matrix
    full_mat <- if (!is.null(full_data)) full_data else NULL
    meta_source <- if (!is.null(gene_data) && !is.null(gene_data$data)) gene_data$data else data.frame()
    row_idx <- if (!is.null(gene_data) && !is.null(gene_data$data)) attr(gene_data$data, "row_idx") else NULL

    # check_exp
    check_exp_input <- input_list[[paste0("check_exp_", id)]]
    if (is.null(check_exp_input)) check_exp_input <- FALSE

    gene_exp <- if (!check_exp_input) get_gene_id(gene_input, gene_data$genes) else gene_input
    if (is.null(gene_exp) || is.na(gene_exp) || gene_exp == "") {
        return(NULL)
    }

    # Identify gene columns from annotation
    gene_cols <- character(0)
    if (!is.null(gene_data$genes) && nrow(gene_data$genes) > 0 && "gene" %in% colnames(gene_data$genes)) {
        gene_cols <- gene_data$genes$gene
    }

    # Determine metadata columns from the filtered metadata (meta_source)
    meta_cols <- setdiff(colnames(meta_source), gene_cols)
    embed_patterns <- c("^umap", "^pca", "^t_sne", "^tsne", "^tSNE")
    embed_regex <- paste(embed_patterns, collapse = "|")
    meta_cols <- meta_cols[!grepl(embed_regex, meta_cols, ignore.case = TRUE)]

    group_col <- group_input
    if (!(group_col %in% meta_cols)) meta_cols <- unique(c(meta_cols, group_col))

    # Build cache key for metadata and check cache first (if provided)
    row_idx_len <- NA
    if (!is.null(gene_data) && !is.null(gene_data$data)) row_idx_len <- length(attr(gene_data$data, "row_idx"))
    cache_key_meta <- make_cache_key("slim_meta", row_idx_len)
    if (!is.null(cache_env) && exists(cache_key_meta, envir = cache_env, inherits = FALSE)) {
        slim_df <- cache_env[[cache_key_meta]]
    } else {
        slim_df <- meta_source %>% select(any_of(meta_cols))
        if (!is.null(cache_env)) {
            try(cache_env[[cache_key_meta]] <- slim_df, silent = TRUE)
        }
    }

    # Caching for full slim_df
    cache_key <- make_cache_key("slim_df", gene_exp, group_col, row_idx_len, id)
    if (!is.null(cache_env) && exists(cache_key, envir = cache_env, inherits = FALSE)) {
        return(cache_env[[cache_key]])
    }

    slim_df <- build_plot_slim(slim_df, full_mat, gene_exp, group_col)

    obj_size <- object.size(slim_df)
    invisible(gc(verbose = FALSE))

    if (!is.null(cache_env)) try(cache_env[[cache_key]] <- slim_df, silent = TRUE)

    return(slim_df)
}

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
