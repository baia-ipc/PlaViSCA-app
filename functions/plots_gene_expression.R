# Gene expression plots (violin plots, one or several genes).

# Function to create multi-gene violin plots
create_multi_gene_plot <- function(gene_data_list, x_col) {
    # gene_data_list is a named list where each element is a data frame for one gene
    # Each data frame should have columns: gene_expr, <x_col>, gene_name

    if (length(gene_data_list) == 0) {
        return(plotly::plot_ly() %>%
            plotly::add_trace(x = c(), y = c(), type = "scatter", mode = "markers") %>%
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
        p <- tryCatch(
            {
                gene_exp_plot(gene_df, x_col, add_title = gene_name)
            },
            error = function(e) {
                plotly::plot_ly() %>%
                    plotly::add_trace(x = c(), y = c(), type = "scatter", mode = "markers") %>%
                    plotly::layout(title = paste(gene_name, "(error)"))
            }
        )

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
            plotly::add_trace(x = c(), y = c(), type = "scatter", mode = "markers") %>%
            plotly::layout(title = "", showlegend = FALSE))
    }

    # Analyze data diversity to provide user feedback
    diversity_analysis <- analyze_data_diversity(clean_data, actual_col)

    # Get unique groups
    unique_groups <- unique(clean_data[[actual_col]])
    n_groups <- length(unique_groups)

    # The plot has no title on purpose
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

    # Calculate statistics for each group for hover text - DO THIS ON FULL DATA
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

    # PERFORMANCE OPTIMIZATION: Downsample for visualization if too many points
    # But keep the full data for stats shown in tooltips
    original_nrow <- nrow(clean_data)
    if (original_nrow > 5000) {
        set.seed(42) # For reproducibility
        # Sample proportionally from each group to maintain distribution shape
        clean_data <- clean_data %>%
            group_by(!!sym(actual_col)) %>%
            slice_sample(prop = 5000 / original_nrow) %>%
            ungroup()
    }

    # Convert back to data.frame after dplyr operations
    clean_data <- as.data.frame(clean_data)

    # Create violin plot for gene expression distribution visualization
    # Ensure we have a regular data.frame, not tibble

    # Validate that required columns exist
    if (!all(c(actual_col, "gene_expr", "mean_val", "sd_val", "count") %in% colnames(clean_data))) {
        return(plotly::plot_ly() %>%
            plotly::add_trace(x = c(), y = c(), type = "scatter", mode = "markers") %>%
            plotly::layout(title = "Missing required columns", showlegend = FALSE))
    }

    plot_df <- data.frame(
        group_var = as.character(clean_data[[actual_col]]),
        gene_expr = as.numeric(clean_data$gene_expr),
        hover_text = paste(
            "Group:", clean_data[[actual_col]],
            "<br>Expression:", round(clean_data$gene_expr, 3),
            "<br>Mean (Full Data):", clean_data$mean_val,
            "<br>SD (Full Data):", clean_data$sd_val,
            "<br>Cells in group:", clean_data$count
        ),
        stringsAsFactors = FALSE
    )

    # Verify we have data and it's valid
    if (nrow(plot_df) == 0 || all(is.na(plot_df$gene_expr)) || all(is.na(plot_df$group_var))) {
        return(plotly::plot_ly() %>%
            plotly::add_trace(x = c(), y = c(), type = "scatter", mode = "markers") %>%
            plotly::layout(title = "", showlegend = FALSE))
    }

    # Remove any remaining NA values
    plot_df <- plot_df[!is.na(plot_df$gene_expr) & !is.na(plot_df$group_var), ]

    if (nrow(plot_df) == 0) {
        return(plotly::plot_ly() %>%
            plotly::add_trace(x = c(), y = c(), type = "scatter", mode = "markers") %>%
            plotly::layout(title = "", showlegend = FALSE))
    }

    # Create the plot by adding one trace per group
    p <- plotly::plot_ly()

    # Recalculate unique groups from cleaned plot_df
    plot_groups <- unique(plot_df$group_var)

    # Sort groups for consistent display
    plot_groups <- sort(plot_groups)

    for (i in seq_along(plot_groups)) {
        group_name <- as.character(plot_groups[i])
        group_idx <- which(plot_df$group_var == group_name)

        if (length(group_idx) > 0) {
            # Extract data for this group
            x_data <- plot_df$group_var[group_idx]
            y_data <- plot_df$gene_expr[group_idx]
            text_data <- plot_df$hover_text[group_idx]

            # Get color for this group
            color_idx <- match(group_name, as.character(unique_groups))
            if (is.na(color_idx)) color_idx <- ((i - 1) %% length(colors)) + 1
            color_val <- colors[min(color_idx, length(colors))]

            p <- plotly::add_trace(
                p,
                x = x_data,
                y = y_data,
                type = "violin",
                name = group_name,
                box = list(visible = TRUE),
                meanline = list(visible = TRUE),
                marker = list(
                    color = color_val,
                    size = if (original_nrow > 5000) 2 else 3,
                    opacity = if (original_nrow > 5000) 0.5 else 0.7
                ),
                line = list(color = color_val),
                fillcolor = color_val,
                points = "all",
                jitter = 0.3,
                pointpos = 0,
                scalemode = "count",
                side = "both",
                bandwidth = 0.2,
                text = text_data,
                hoverinfo = "text",
                showlegend = FALSE
            )
        }
    }

    p <- p %>%
        plotly::layout(
            yaxis = list(title = "Exp. value"),
            xaxis = list(
                title = "",
                type = "category",
                categoryorder = "array",
                categoryarray = plot_groups
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
