# Plots of the QC Plots tab.

# Set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

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
