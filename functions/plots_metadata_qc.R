# Plots of the Plots and QC Plots tabs.

# Set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

# Generate metadata plot
#
# `show_missing = TRUE` keeps cells whose value is truly missing and displays
# them under the configured missing label instead of silently dropping them.
meta_plotly <- function(data, x_var, x_label, y_label, plt_type = "bar",
                        show_missing = TRUE) {
    if (!x_var %in% colnames(data)) {
        stop(sprintf(
            paste0(
                "PlaViSca data error: column '%s' is not present in the ",
                "loaded data. The data do not match config/data_columns.yml; ",
                "this is not caused by user input."
            ),
            x_var
        ), call. = FALSE)
    }

    if (show_missing) {
        # Display-only relabelling: the underlying data is untouched.
        data[[x_var]] <- relabel_missing(data[[x_var]], missing_label_for(x_var))
    } else {
        data <- data[!is_missing_value(data[[x_var]]), , drop = FALSE]
    }

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
        summarise(count = n(), .groups = "drop")

    # Biological (config-driven) ordering where the field has one; otherwise
    # numeric/alphabetical. Missing always sorts last.
    missing_label <- missing_label_for(x_var)
    ordered_levels <- order_column_values(
        x_var, setdiff(data_summary[[x_var]], missing_label)
    )
    if (missing_label %in% data_summary[[x_var]]) {
        ordered_levels <- c(ordered_levels, missing_label)
    }
    # plotly draws horizontal bars bottom-up, so reverse for top-down reading
    ordered_levels <- rev(ordered_levels)
    data_summary <- data_summary[match(ordered_levels, data_summary[[x_var]]), , drop = FALSE]
    data_summary <- data_summary[!is.na(data_summary[[x_var]]), , drop = FALSE]

    # Format species names with italics for configured scientific-name fields
    if (x_var %in% italicised_columns()) {
        data_summary$formatted_labels <- format_species_name(data_summary[[x_var]], x_var)
    } else {
        data_summary$formatted_labels <- display_values(x_var, data_summary[[x_var]])
    }
    data_summary$formatted_labels <- factor(
        data_summary$formatted_labels,
        levels = unique(data_summary$formatted_labels)
    )

    data_summary[[x_var]] <- factor(
        data_summary[[x_var]],
        levels = data_summary[[x_var]]
    )

    colors_vec <- category_palette(levels(data_summary[[x_var]]), x_var)

    plot_ly(
        data_summary,
        x = ~count,
        y = ~formatted_labels,
        color = ~ .data[[x_var]],
        colors = colors_vec,
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
