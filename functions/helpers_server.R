# Helper function for creating safe cache keys
# Helper functions for server-side operations

# Function to create multi-gene violin plots
create_multi_gene_plot <- function(gene_data_list, x_col) {
    # gene_data_list is a named list where each element is a data frame for one gene
    # Each data frame should have columns: gene_expr, <x_col>, gene_name
    
    if (length(gene_data_list) == 0) {
        return(plotly::plot_ly() %>% 
               plotly::add_trace(x = c(), y = c(), type = 'scatter', mode = 'markers') %>%
               plotly::layout(title = "", showlegend = FALSE))
    }
    
    # Create subplot layout based on number of genes
    n_genes <- length(gene_data_list)
    
    if (n_genes == 2) {
        nrows <- 1
        ncols <- 2
    } else if (n_genes == 3) {
        nrows <- 1
        ncols <- 3
    } else {
        nrows <- 2
        ncols <- 2
    }
    
    # Create individual plots for each gene
    plot_list <- list()
    
    for (i in seq_along(gene_data_list)) {
        gene_name <- names(gene_data_list)[i]
        gene_df <- gene_data_list[[i]]
        
        # Create plot for this gene with title
        p <- tryCatch({
            gene_exp_plot(gene_df, x_col, add_title = gene_name)
        }, error = function(e) {
            plotly::plot_ly() %>% 
                plotly::add_trace(x = c(), y = c(), type = 'scatter', mode = 'markers') %>%
                plotly::layout(title = paste(gene_name, "(error)"))
        })
        
        plot_list[[i]] <- p
    }
    
    # Combine into subplot
    subplot_args <- c(plot_list, list(
        nrows = nrows,
        shareX = TRUE,
        shareY = FALSE,
        titleX = TRUE,
        titleY = TRUE,
        margin = 0.05
    ))
    
    combined_plot <- do.call(plotly::subplot, subplot_args)
    
    return(combined_plot)
}

# Cache key generator
make_cache_key <- function(...) {
    args <- list(...)
    parts <- lapply(args, function(x) {
        if (is.null(x)) {
            return("NULL")
        } else if (length(x) > 1) {
            return(paste(sort(as.character(x)), collapse = "|"))
        } else {
            return(as.character(x))
        }
    })
    paste(parts, collapse = "__")
}

# Create a cache key for filtered gene expression inputs
filtered_gene_exp_cache_key <- function(input) {
    get_field <- function(name) {
        if (is.null(input)) {
            return(NULL)
        }
        if (is.list(input) && !is.null(input[[name]])) {
            return(input[[name]])
        }
        tryCatch(
            input[[name]],
            error = function(e) NULL
        )
    }

    make_cache_key(
        "filtered_gene_exp",
        get_field("study_label_exp"),
        get_field("host_species_exp"),
        get_field("life_cycle_stage_exp"),
        get_field("treatment_exp"),
        get_field("parasite_stage_exp"),
        get_field("sample_type_exp"),
        get_field("strain_exp"),
        get_field("hour_post_invasion_exp"),
        get_field("sequencer_exp"),
        get_field("sc_technology_exp")
    )
}

# Helper function to sync data types between tabs
sync_data_type <- function(
    session,
    from_input,
    to_input,
    new_value,
    global_loading,
    delay = 1.5) {
    global_loading(TRUE)
    updateRadioButtons(
        session,
        to_input,
        selected = new_value
    )
    later(
        function() {
            tryCatch(
                {
                    global_loading(FALSE)
                },
                error = function(e) {
                    # Silent error handling
                }
            )
        },
        delay
    )
}

# Helper function to create data type
create_data_type_sync_observer <- function(
    input,
    session,
    input_name,
    target_input,
    expected_tabset,
    global_loading) {
    observeEvent(
        input[[input_name]],
        {
            if (
                !is.null(input[[input_name]]) &&
                    !is.null(input$tabset) &&
                    input$tabset == expected_tabset
            ) {
                sync_data_type(
                    session = session,
                    from_input = input_name,
                    to_input = target_input,
                    new_value = input[[input_name]],
                    global_loading = global_loading
                )
            }
        },
        ignoreInit = TRUE
    )
}

# Helper function to analyze data homogeneity and provide user feedback
analyze_data_diversity <- function(data, group_col) {
    if (is.null(data) || nrow(data) == 0 || !group_col %in% colnames(data)) {
        return(list(
            is_homogeneous = TRUE,
            unique_count = 0,
            message = "No data available for grouping analysis",
            suggestion = "Try different filter combinations"
        ))
    }
    
    # Remove NA values for analysis
    clean_values <- data[[group_col]][!is.na(data[[group_col]])]
    unique_values <- unique(clean_values)
    unique_count <- length(unique_values)
    
    # Determine if data is homogeneous (only 1 unique value)
    is_homogeneous <- unique_count <= 1
    
    if (is_homogeneous) {
        if (unique_count == 0) {
            message <- paste("No valid data found for", group_col)
            suggestion <- "Check your filter settings"
        } else {
            value <- unique_values[1]
            total_cells <- nrow(data)
            message <- paste0("All ", format(total_cells, big.mark=","), " cells have the same ", 
                            gsub("_", " ", group_col), ": '", value, "'")
            
            # Provide context-specific suggestions
            if (group_col == "life_cycle_stage") {
                suggestion <- "This is biologically correct - try grouping by host_species, sample_type, or study_label instead"
            } else if (group_col == "parasite_stage") {
                suggestion <- "This is expected when filtering by parasite stage - try life_cycle_stage or host_species grouping"
            } else if (group_col == "host_species") {
                suggestion <- "This filtered dataset contains data from only one host species - try sample_type or study_label grouping"
            } else if (group_col == "sample_type") {
                suggestion <- "This filtered dataset contains only one sample type - try host_species or strain grouping"
            } else {
                suggestion <- "Try different grouping categories or adjust your filters to include more diverse data"
            }
        }
    } else {
        message <- paste0("Found ", unique_count, " different ", gsub("_", " ", group_col), " categories")
        suggestion <- ""
    }
    
    return(list(
        is_homogeneous = is_homogeneous,
        unique_count = unique_count,
        unique_values = if(unique_count <= 10) unique_values else paste0(length(unique_values), " categories"),
        message = message,
        suggestion = suggestion,
        total_cells = nrow(data)
    ))
}

# Helper function to format species names with italics
format_species_name <- function(species_names) {
    scientific_species <- c(
        "Anopheles freeborni",
        "Anopheles stephensi",
        "Anopheles dirus",
        "Saimiri boliviensis",
        "Aotus nancymaae",
        "Homo sapiens"
    )

    # Apply italic formatting to scientific names
    formatted_names <- ifelse(
        species_names %in% scientific_species,
        paste0("<i>", species_names, "</i>"),
        species_names
    )

    return(formatted_names)
}

fun_filter_df <- function(data, selections) {
    if (is.null(data)) {
        return(NULL)
    }
    if (is.null(selections) || length(selections) == 0) {
        return(data)
    }

    # Parse selections safely
    parts <- strsplit(selections, "__", fixed = TRUE)
    valid_parts <- Filter(function(x) length(x) == 2, parts)
    if (length(valid_parts) == 0) {
        return(data)
    }

    # Build filter list
    filter_list <- lapply(
        split(valid_parts, vapply(valid_parts, `[[`, character(1), 1)),
        function(lst) vapply(lst, `[[`, character(1), 2)
    )

    keep_rows <- rep(TRUE, nrow(data))
    for (colname in names(filter_list)) {
        if (colname %in% names(data)) {
            keep_rows <- keep_rows & data[[colname]] %in% filter_list[[colname]]
        } else {
            warning(sprintf(
                "Column '%s' not found in data; skipping filter.",
                colname
            ))
        }
    }

    # Always return with the same columns as input
    out <- data[keep_rows, , drop = FALSE]
    if (nrow(out) == 0) {
        out <- data[0, , drop = FALSE]
    }

    # subset the full expression matrix by row without re-running filters.
    attr(out, "row_idx") <- which(keep_rows)
    return(out)
}


# Helper function to apply secondary filters
apply_secondary_filters <- function(data, inputs, keep_gene_matrix = TRUE, genes = NULL) {
    
    filtered <- data
    if ("n_feature_rna" %in% names(data) && "n_count_rna" %in% names(data)) {
        filtered <- filtered %>%
            filter(n_feature_rna > 0 & n_count_rna > 0)
    }

    # Select relevant columns
    metadata_cols <- c(
        "study_label",
        "host_species",
        "life_cycle_stage",
        "treatment",
        "parasite_stage",
        "sample_type",
        "geographic_location",
        "sc_technology",
        "sequencer",
        "strain",
        "day_post_infection",
        "hour_post_invasion",
        "biological_replicate",
        "n_count_rna",
        "n_feature_rna"
    )

    # For gene expression data, keep all columns (metadata + gene expression)
    # For regular metadata, select only metadata columns
    available_metadata_cols <- intersect(metadata_cols, names(filtered))
    non_metadata_cols_count <- length(setdiff(names(filtered), metadata_cols))

    if (!keep_gene_matrix) {
        if (!is.null(genes) && length(genes) > 0) {
            keep_cols <- setdiff(names(filtered), genes)
            keep_cols <- intersect(keep_cols, names(filtered))
            keep_cols <- unique(c(keep_cols, available_metadata_cols))
            filtered <- filtered %>% select(any_of(keep_cols))
        } else {
            filtered <- filtered %>% select(any_of(metadata_cols))
        }
    } else {
        if (non_metadata_cols_count > 10) {
            filtered <- filtered
        } else {
            filtered <- filtered %>% select(any_of(metadata_cols))
        }
    }



    # Dynamically apply filters for each non-empty input (on the filtered view)
    for (col in names(inputs)) {
        values <- inputs[[col]]
        if (!is.null(values) && length(values) > 0 && col %in% names(filtered)) {
            filtered <- filtered %>% filter(.data[[col]] %in% values)
        }
    }

    # Create row indices based on original data before any column selection/filtering
    original <- data
    if (!is.null(original) && nrow(original) > 0) {
        keep_rows <- rep(TRUE, nrow(original))

        # Apply the RNA QC filters if present
        if ("n_feature_rna" %in% names(original) && "n_count_rna" %in% names(original)) {
            keep_rows <- keep_rows & (original$n_feature_rna > 0 & original$n_count_rna > 0)
        }

        # Apply the dynamic inputs - THIS IS THE KEY FILTERING STEP
        if (!is.null(inputs) && length(inputs) > 0) {
            for (col in names(inputs)) {
                values <- inputs[[col]]
                if (!is.null(values) && length(values) > 0 && col %in% names(original)) {
                    keep_rows <- keep_rows & (original[[col]] %in% values)
                }
            }
        }

        # Attach row indices (positions in the original data)
        idx <- which(keep_rows)
        attr(filtered, "row_idx") <- idx
    } else {
        attr(filtered, "row_idx") <- integer(0)
    }

    return(filtered)
}


# Get genes annotation
genes <- function(data, gff_data, pattern = "pvp") {
    data %>%
        colnames() %>%
        grep(pattern, ., ignore.case = TRUE, value = TRUE) %>%
        as.data.frame() %>%
        setNames("gene") %>%
        left_join(gff_data, by = c("gene" = "id")) %>%
        mutate(name = coalesce(name, gene)) %>%
        select(gene, name)
}

# Get gene column
get_gene_choices <- function(genes_df, checked) {
    if (is.null(genes_df) || nrow(genes_df) == 0) {
        return(NULL)
    }

    if (checked) {
        genes_df$gene
    } else {
        # Use base R instead of stringr to avoid dependency issues
        genes_df %>%
            filter(!grepl("^PVP01", name)) %>%
            pull(name)
    }
}

# Get gene id
get_gene_id <- function(gene_name, gene_names) {
    # Handle NULL or empty input
    if (is.null(gene_name) || length(gene_name) == 0) {
        return(NA)
    }
    
    # Handle vector input - return only the first match
    if (length(gene_name) > 1) {
        gene_name <- gene_name[1]
    }
    
    # Handle NA or empty string
    if (is.na(gene_name) || gene_name == "") {
        return(NA)
    }

    if (gene_name %in% gene_names$name) {
        result <- gene_names$gene[gene_names$name == gene_name]
        return(result[1])  # Return only first match
    } else {
        return(NA)
    }
}


# Generate df for gene_exp0
df_exp0 <- function(gene_data, gene_input, group_input, check_exp, full_data = NULL, cache_env = NULL) {
    full_mat <- if (!is.null(full_data)) full_data else NULL
    meta_source <- if (!is.null(gene_data) && !is.null(gene_data$data)) gene_data$data else data.frame()
    row_idx <- if (!is.null(gene_data) && !is.null(gene_data$data)) attr(gene_data$data, "row_idx") else NULL
    genes <- gene_data$genes

    # CRITICAL FIX: Force use of filtered data when row_idx is available
    # The first issue is sometimes row_idx is wrong (pointing to full dataset)
    if (!is.null(row_idx) && length(row_idx) > 0) {
        # Check if row_idx makes sense - if it's the same length as meta_source,
        # then either we have the full dataset OR the filtering didn't work
        if (length(row_idx) == nrow(meta_source) && length(row_idx) > 10000) {
            # This looks like the full dataset, which means row_idx is wrong
            row_idx <- NULL
            attr(meta_source, "row_idx") <- NULL
        } else if (nrow(meta_source) != length(row_idx)) {
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

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

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

# Set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

# Generate metadata plot
meta_plotly <- function(data, x_var, x_label, y_label, plt_type = "bar") {
    if (!x_var %in% colnames(data)) {
        stop(paste("Column", x_var, "not found in data"))
    }

    # Remove rows with missing values in the x_var column
    data <- data %>%
        filter(!is.na(.data[[x_var]]) & .data[[x_var]] != "")

    # Check if we have any data left after filtering
    if (nrow(data) == 0) {
        return(
            plot_ly() %>%
                add_annotations(
                    text = paste("No data available for", x_label),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5
                ) %>%
                layout(
                    xaxis = list(title = x_label),
                    yaxis = list(title = y_label),
                    showlegend = FALSE
                )
        )
    }

    data_summary <- data %>%
        group_by(.data[[x_var]]) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(count)

    # Format species names with italics if this is host_species column
    if (x_var == "host_species") {
        data_summary$formatted_labels <- format_species_name(data_summary[[x_var]])
    } else {
        data_summary$formatted_labels <- data_summary[[x_var]]
    }

    data_summary[[x_var]] <- factor(
        data_summary[[x_var]],
        levels = data_summary[[x_var]]
    )

    plot_ly(
        data_summary,
        x = ~count,
        y = ~formatted_labels,
        color = ~ .data[[x_var]],
        colors = brewer.pal(
            n = length(unique(data_summary[[x_var]])),
            name = "Set3"
        ),
        type = plt_type
    ) %>%
        layout(
            xaxis = list(title = x_label),
            yaxis = list(
                title = y_label,
                ticklen = 5,
                tickwidth = 2,
                tickcolor = "transparent"
            ),
            showlegend = FALSE,
            barmode = "group",
            bargap = 0.1,
            margin = list(l = 180, r = 50, t = 50, b = 80)
        )
}

# Generate plot for RNA and feature count
feature_plot <- function(df, x, y, color, y_title, title) {
    req(df)
    plot_ly(
        data = df,
        x = ~ get(x),
        y = ~ get(y),
        color = ~ get(color),
        colors = colors,
        type = "violin",
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        jitter = 0.4
    ) %>%
        layout(
            xaxis = list(title = x),
            yaxis = list(title = y_title),
            legend = list(title = list(text = color))
        )
}

# Generate Elbow plot for PCA
elbow_plot <- function(pca_df, selected_study) {
    req(pca_df)

    pca_df <- pca_df %>%
        filter(study == !!selected_study)

    plot_ly(
        data = pca_df,
        x = ~pc,
        y = ~std,
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 8, color = "red"),
        line = list(color = "blue"),
        text = ~ paste("PCA:", pc, "<br>Stdv:", std),
        hoverinfo = "text"
    ) %>%
        layout(
            xaxis = list(title = "PCA"),
            yaxis = list(title = "Standard Deviation"),
            showlegend = FALSE
        )
}

# Generate plot for top variable genes
var_genes_plot <- function(df, study, top_n = 10) {
    req(df)

    df <- df %>%
        filter(study == !!study)

    counts <- table(df$variable)
    var_count <- counts["TRUE"]
    nonvar_count <- counts["FALSE"]

    # Create separate datasets for each category
    non_variable_genes <- df %>% filter(variable == FALSE)
    variable_genes <- df %>% filter(variable == TRUE)

    top_hvf <- variable_genes %>%
        arrange(rank) %>%
        head(top_n)

    # Remove top genes from regular variable genes to avoid overlap
    regular_variable_genes <- variable_genes %>%
        anti_join(top_hvf, by = "name")

    plot_ly() %>%
        add_trace(
            data = non_variable_genes,
            x = ~mean,
            y = ~variance.standardized,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, opacity = 0.7, color = "black"),
            text = ~name,
            hovertemplate = paste(
                "Gene: %{text}<br>",
                "Mean: %{x}<br>",
                "Variance: %{y}<extra></extra>"
            ),
            name = paste0("Non-variable (n = ", nonvar_count, ")"),
            showlegend = TRUE
        ) %>%
        # Regular variable genes (red)
        add_trace(
            data = regular_variable_genes,
            x = ~mean,
            y = ~variance.standardized,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, opacity = 0.7, color = "red"),
            text = ~name,
            hovertemplate = paste(
                "Gene: %{text}<br>",
                "Mean: %{x}<br>",
                "Variance: %{y}<extra></extra>"
            ),
            name = paste0("Variable (n = ", var_count, ")"),
            showlegend = TRUE
        ) %>%
        # Top variable genes (orange)
        add_trace(
            data = top_hvf,
            x = ~mean,
            y = ~variance.standardized,
            type = "scatter",
            mode = "markers",
            marker = list(size = 6, opacity = 1, color = "orange"),
            text = ~name,
            hovertemplate = paste(
                "Gene: %{text}<br>",
                "Mean: %{x}<br>",
                "Variance: %{y}<extra></extra>"
            ),
            name = paste("Top", top_n, "Variable"),
            showlegend = TRUE
        ) %>%
        layout(
            xaxis = list(title = "Mean Expression"),
            yaxis = list(title = "Standardized Variance"),
            legend = list(title = list(text = "<b>Variable Status</b>"))
        )
}


# Generate violin plot for gene expression data
gene_exp_plot <- function(data, x_col, add_title = NULL) {
    actual_col <- colnames(data)[tolower(colnames(data)) == tolower(x_col)]

    if (length(actual_col) == 0) {
        stop(paste(
            "Column",
            x_col,
            "not found in data. Available columns:",
            paste(colnames(data), collapse = ", ")
        ))
    }

    # Use the first match
    actual_col <- actual_col[1]

    # Ensure gene_expr column exists
    if (!"gene_expr" %in% colnames(data)) {
        stop("gene_expr column not found in data")
    }

    # Remove any rows with missing values to avoid plotly issues
    clean_data <- data[complete.cases(data[c(actual_col, "gene_expr")]), ]
    
    # Convert to regular data.frame (not tibble) to avoid compatibility issues
    clean_data <- as.data.frame(clean_data)
    
    if (nrow(clean_data) == 0) {
        return(plotly::plot_ly() %>%
               plotly::add_trace(x = c(), y = c(), type = 'scatter', mode = 'markers') %>% 
               plotly::layout(title = "", showlegend = FALSE))
    }

    # Analyze data diversity to provide user feedback
    diversity_analysis <- analyze_data_diversity(clean_data, actual_col)
    
    # Get unique groups
    unique_groups <- unique(clean_data[[actual_col]])
    n_groups <- length(unique_groups)

    # Create plot title - removed diversity message per user request
    plot_title <- ""

    # Use Set2 palette for better colors - ensure unnamed vector
    colors <- if (n_groups <= 8) {
        unname(RColorBrewer::brewer.pal(max(3, n_groups), "Set2")[1:n_groups])
    } else {
        # Use viridis if available, otherwise fall back to rainbow colors
        if (requireNamespace("viridis", quietly = TRUE)) {
            unname(viridis::viridis(n_groups))
        } else {
            unname(rainbow(n_groups))
        }
    }

    # Calculate statistics for each group for hover text
    stats_data <- clean_data %>%
        group_by(!!sym(actual_col)) %>%
        summarise(
            mean_val = round(mean(gene_expr, na.rm = TRUE), 3),
            sd_val = round(sd(gene_expr, na.rm = TRUE), 3),
            count = n(),
            .groups = "drop"
        )

    # Merge statistics back to the main data for hover text
    clean_data <- clean_data %>%
        left_join(stats_data, by = actual_col)
    
    # Convert back to data.frame after dplyr operations
    clean_data <- as.data.frame(clean_data)

    # Create violin plot for gene expression distribution visualization
    # Ensure we have a regular data.frame, not tibble
    
    # Validate that required columns exist
    if (!all(c(actual_col, "gene_expr", "mean_val", "sd_val", "count") %in% colnames(clean_data))) {
        return(plotly::plot_ly() %>% 
               plotly::add_trace(x = c(), y = c(), type = 'scatter', mode = 'markers') %>%
               plotly::layout(title = "Missing required columns", showlegend = FALSE))
    }
    
    plot_df <- data.frame(
        group_var = as.character(clean_data[[actual_col]]),
        gene_expr = as.numeric(clean_data$gene_expr),
        hover_text = paste(
            "Group:", clean_data[[actual_col]],
            "<br>Expression:", round(clean_data$gene_expr, 3),
            "<br>Mean:", clean_data$mean_val,
            "<br>SD:", clean_data$sd_val,
            "<br>Cells in group:", clean_data$count
        ),
        stringsAsFactors = FALSE
    )
    
    # Verify we have data and it's valid
    if (nrow(plot_df) == 0 || all(is.na(plot_df$gene_expr)) || all(is.na(plot_df$group_var))) {
        return(plotly::plot_ly() %>% 
               plotly::add_trace(x = c(), y = c(), type = 'scatter', mode = 'markers') %>%
               plotly::layout(title = "", showlegend = FALSE))
    }
    
    # Remove any remaining NA values
    plot_df <- plot_df[!is.na(plot_df$gene_expr) & !is.na(plot_df$group_var), ]
    
    if (nrow(plot_df) == 0) {
        return(plotly::plot_ly() %>% 
               plotly::add_trace(x = c(), y = c(), type = 'scatter', mode = 'markers') %>%
               plotly::layout(title = "", showlegend = FALSE))
    }
    
    # Create the plot by adding one trace per group
    p <- plotly::plot_ly()
    
    # Recalculate unique groups from cleaned plot_df
    plot_groups <- unique(plot_df$group_var)
    
    for (i in seq_along(plot_groups)) {
        group_name <- as.character(plot_groups[i])
        group_idx <- plot_df$group_var == group_name
        
        if (sum(group_idx, na.rm = TRUE) > 0) {
            # Extract data for this group
            x_data <- plot_df$group_var[group_idx]
            y_data <- plot_df$gene_expr[group_idx]
            text_data <- plot_df$hover_text[group_idx]
            
            # Only add trace if we have valid data
            if (length(x_data) > 0 && length(y_data) > 0 && !all(is.na(y_data))) {
                # Get color for this group (map back to original unique_groups if possible)
                color_idx <- match(group_name, as.character(unique_groups))
                if (is.na(color_idx) || color_idx > length(colors)) {
                    color_idx <- ((i - 1) %% length(colors)) + 1
                }
                
                p <- plotly::add_trace(
                    p,
                    x = x_data,
                    y = y_data,
                    type = "violin",
                    name = group_name,
                    box = list(visible = TRUE),
                    meanline = list(visible = TRUE),
                    marker = list(
                        color = colors[color_idx],
                        size = 3,
                        opacity = 0.7
                    ),
                    line = list(color = colors[color_idx]),
                    fillcolor = colors[color_idx],
                    points = "all",          # Show all points
                    jitter = 0.3,            # Add jitter for point separation
                    pointpos = 0,            # Center points inside violin (0 = inside)
                    scalemode = "count",     # Scale violin by count for better comparison
                    side = "both",           # Show both sides of violin
                    bandwidth = 0.2,         # Adjust bandwidth for smoother curves
                    text = text_data,
                    hoverinfo = "text",
                    showlegend = FALSE
                )
            }
        }
    }
    
    p <- p %>%
        plotly::layout(
            yaxis = list(title = "Exp. value"),
            xaxis = list(
                title = "",
                type = "category"
            ),
            title = if (!is.null(add_title)) {
                list(text = add_title, font = list(size = 14))
            } else {
                NULL
            },
            showlegend = FALSE
        )

    return(p)
}


# Function to generate data summary for filtered datasets
generate_data_summary <- function(data, filters_applied = NULL) {
    if (is.null(data) || nrow(data) == 0) {
        return("No data available")
    }
    
    total_cells <- nrow(data)
    
    # Key metadata columns to summarize
    summary_cols <- c("study_label", "host_species", "sample_type", "parasite_stage", 
                     "life_cycle_stage", "strain", "geographic_location", "treatment")
    
    available_cols <- intersect(summary_cols, colnames(data))
    
    summary_parts <- c()
    summary_parts <- c(summary_parts, paste("📊 <b>Total cells:</b>", format(total_cells, big.mark=",")))
    
    for (col in available_cols) {
        if (col %in% colnames(data)) {
            unique_vals <- unique(data[[col]][!is.na(data[[col]])])
            count <- length(unique_vals)
            col_display <- gsub("_", " ", col)
            col_display <- paste0(toupper(substring(col_display, 1, 1)), substring(col_display, 2))
            
            if (count == 0) {
                summary_parts <- c(summary_parts, paste("•", col_display, ": No data"))
            } else if (count == 1) {
                summary_parts <- c(summary_parts, paste("•", col_display, ":", unique_vals[1]))
            } else if (count <= 3) {
                summary_parts <- c(summary_parts, paste("•", col_display, "(", count, "):", paste(unique_vals, collapse=", ")))
            } else {
                summary_parts <- c(summary_parts, paste("•", col_display, ":", count, "different values"))
            }
        }
    }
    
    if (!is.null(filters_applied) && length(filters_applied) > 0) {
        filter_text <- paste(names(filters_applied), "=", sapply(filters_applied, paste, collapse=","), collapse="; ")
        summary_parts <- c(summary_parts, paste("🔍 <b>Filters applied:</b>", filter_text))
    }
    
    return(paste(summary_parts, collapse="<br>"))
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
        # Single gene - original logic
        if (!is.null(full_mat) && !is.null(gene_exp) && gene_exp %in% colnames(full_mat)) {
            if (!is.null(row_idx) && length(row_idx) > 0) {
                # Use row indices - this is the correct filtered subset
                gene_vec <- full_mat[[gene_exp]][row_idx]
            } else {
                # Fallback: use sequential indices (this might be wrong for filtered data)
                gene_vec <- full_mat[[gene_exp]]
                if (nrow(slim) > 0) gene_vec <- gene_vec[seq_len(nrow(slim))]
            }
        } else {
            gene_vec <- rep(NA, nrow(slim))
        }
    }

    # CRITICAL FIX: Ensure gene expression vector length matches slim dataframe
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
