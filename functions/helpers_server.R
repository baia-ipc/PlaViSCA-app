# Helper function for creating safe cache keys
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

    original <- data
    if (!is.null(original) && nrow(original) > 0) {
        keep_rows <- rep(TRUE, nrow(original))

        # Apply the RNA QC filters if present
        if ("n_feature_rna" %in% names(original) && "n_count_rna" %in% names(original)) {
            keep_rows <- keep_rows & (original$n_feature_rna > 0 & original$n_count_rna > 0)
        }

        # Apply the dynamic inputs
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
        genes_df %>%
            filter(!str_detect(name, "^PVP01")) %>%
            pull(name)
    }
}

# Get gene id
get_gene_id <- function(gene_name, gene_names) {
    if (is.null(gene_name) || is.na(gene_name) || gene_name == "") {
        return(NA)
    }

    if (gene_name %in% gene_names$name) {
        return(gene_names$gene[gene_names$name == gene_name])
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

    gene_exp <- if (!check_exp) {
        get_gene_id(gene_input, genes)
    } else {
        gene_input
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

    row_idx_len <- NA
    if (!is.null(gene_data) && !is.null(gene_data$data)) row_idx_len <- length(attr(gene_data$data, "row_idx"))
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


# Generate violin plot with jittered points overlay
gene_exp_plot <- function(data, x_col) {
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

    if (nrow(clean_data) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = ""))
    }

    # Get unique groups
    unique_groups <- unique(clean_data[[actual_col]])
    n_groups <- length(unique_groups)

    # Use Set2 palette for better colors
    colors <- if (n_groups <= 8) {
        RColorBrewer::brewer.pal(max(3, n_groups), "Set2")[1:n_groups]
    } else {
        viridis::viridis(n_groups)
    }

    # Calculate statistics for each group for hover text
    stats_data <- clean_data %>%
        group_by(!!sym(actual_col)) %>%
        summarise(
            mean_val = round(mean(gene_expr, na.rm = TRUE), 3),
            sd_val = round(sd(gene_expr, na.rm = TRUE), 3),
            .groups = "drop"
        )

    # Merge statistics back to the main data for hover text
    clean_data <- clean_data %>%
        left_join(stats_data, by = actual_col)

    # Create violin plot
    p <- plotly::plot_ly(
        data = clean_data,
        x = clean_data[[actual_col]],
        y = ~gene_expr,
        type = "violin",
        box = list(visible = FALSE),
        meanline = list(visible = FALSE),
        color = clean_data[[actual_col]],
        colors = colors,
        showlegend = FALSE,
        points = "all",
        jitter = 0.3,
        pointpos = 0,
        text = ~ paste(
            "Group:",
            clean_data[[actual_col]],
            "<br>",
            "Expression:",
            round(gene_expr, 3),
            "<br>",
            "Mean:",
            mean_val,
            "<br>",
            "SD:",
            sd_val
        ),
        hoverinfo = "text"
    ) %>%
        plotly::layout(
            yaxis = list(title = "Exp. value")
        )

    return(p)
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
    if (!is.null(full_mat) && !is.null(gene_exp) && gene_exp %in% colnames(full_mat)) {
        if (!is.null(row_idx) && length(row_idx) > 0) {
            gene_vec <- full_mat[[gene_exp]][row_idx]
        } else {
            gene_vec <- full_mat[[gene_exp]]
            if (nrow(slim) > 0) gene_vec <- gene_vec[seq_len(nrow(slim))]
        }
    } else {
        gene_vec <- rep(NA, nrow(slim))
    }

    slim$gene_expr <- gene_vec
    return(slim)
}
