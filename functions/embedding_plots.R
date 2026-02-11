# Embedding plotting helpers

y_col <- function(check_input, x_col, unintegrated, integrated) {
    if (check_input) {
        setdiff(unintegrated, x_col)
    } else {
        setdiff(integrated, x_col)
    }
}

# Generate color column based on input option
get_color_column <- function(color_type, gene_input, gene_data_obj) {
    switch(color_type,
        "genes" = {
            # Handle multiple gene selection by averaging
            if (length(gene_input) > 1) {
                # For multiple genes, return a special identifier that signals averaging
                # The actual averaging will be handled in the server logic
                paste0("multi_gene_avg_", length(gene_input))
            } else {
                get_gene_id(gene_input, gene_data_obj)
            }
        },
        "cca_snn" = gene_input,
        color_type
    )
}


# Assign colors based on the number of categories
assign_colors <- function(num_categories) {
    if (num_categories <= 8) {
        return(RColorBrewer::brewer.pal(
            n = max(num_categories, 3),
            name = "Set2"
        )[1:num_categories])
    } else {
        return(rep(
            RColorBrewer::brewer.pal(8, "Set2"),
            length.out = num_categories
        ))
    }
}


# Check if the data is categorical
is_categorical_data <- function(x) {
    is.character(x) || is.factor(x)
}


# Colors for categorical data
prepare_categorical_colors <- function(df, color) {
    df[[color]] <- as.character(df[[color]])
    
    # Create context-aware NA label
    na_label <- if (color == "blood_stage") {
        "NA (not in blood stage)"
    } else {
        "NA (missing)"
    }
    
    df[[color]][is.na(df[[color]])] <- na_label

    unique_values <- unique(df[[color]])

    has_na <- na_label %in% unique_values
    if (has_na) {
        actual_values <- unique_values[unique_values != na_label]
    } else {
        actual_values <- unique_values
    }

    # Sort the actual values
    if (length(actual_values) > 0) {
        numeric_values <- suppressWarnings(as.numeric(actual_values))
        if (all(!is.na(numeric_values))) {
            sorted_values <- actual_values[order(numeric_values)]
        } else {
            sorted_values <- sort(actual_values)
        }
    } else {
        sorted_values <- character(0)
    }

    final_levels <- if (has_na) {
        c(na_label, sorted_values)
    } else {
        sorted_values
    }

    # Assign colors
    num_actual_colors <- length(sorted_values)
    palette <- if (num_actual_colors > 0) {
        assign_colors(num_actual_colors)
    } else {
        character(0)
    }

    colors <- if (has_na) {
        setNames(c("#A9A9A94D", palette), final_levels)
    } else {
        setNames(palette, final_levels)
    }

    # Crucially, create a factor with the correctly ordered levels
    df[[color]] <- factor(df[[color]], levels = final_levels)

    return(list(df = df, colors = colors))
}

get_dim <- function(plot_type) {
    list(
        pca_u = c("pca_u_1", "pca_u_2", "pca_u_3"),
        pca_i = c("pca_i_1", "pca_i_2", "pca_i_3"),
        umap_i = c("umap_i_1", "umap_i_2", "umap_i_3"),
        umap_u = c("umap_u_1", "umap_u_2", "umap_u_3"),
        tSNE = c("t_sne_1", "t_sne_2", "t_sne_3")
    )[[plot_type]]
}


get_legend_layout <- function(legend_title, num_categories = NULL, is_dual_plot = FALSE, is_gene_plot = FALSE) {
    # Check if this is a gene plot (continuous color scale)
    if (is_gene_plot) {
        # For gene plots, use a smaller, borderless legend
        list(
            title = list(text = legend_title, font = list(size = 12, family = "Arial")),
            orientation = "v",
            x = 0.02,  # Far left of plot area
            xanchor = "left", 
            y = 0.98,  # Top of plot area
            yanchor = "top",
            itemsizing = "constant",
            font = list(size = 11, family = "Arial"),
            bgcolor = "rgba(255,255,255,0.8)",
            borderwidth = 0,  # Remove border
            len = 0.5,  # Make legend half the height
            thickness = 15,  # Make legend thinner
            itemclick = "toggle",
            itemdoubleclick = "toggleothers"
        )
    } else {
        # Always position legend at top-left of plot area, overlaid for consistency
        list(
            title = list(text = legend_title, font = list(size = 14, family = "Arial")),
            orientation = "v",
            x = 0.02,  # Far left of plot area
            xanchor = "left",
            y = 0.98,  # Top of plot area
            yanchor = "top",
            itemsizing = "constant",
            font = list(size = 12, family = "Arial"),
            bgcolor = "rgba(255,255,255,0.95)",
            bordercolor = "rgba(0,0,0,0.5)",
            borderwidth = 1,
            itemclick = "toggle",  # Toggle individual items on/off
            itemdoubleclick = "toggleothers"  # Double-click to show only one
        )
    }
}

# Get appropriate margins based on legend orientation
get_plot_margins <- function(num_categories = NULL, is_dual_plot = FALSE) {
    # Since legend is always overlaid at top-left, use consistent margins for all plots
    list(l = 50, r = 50, t = 40, b = 40)
}


# Hover text for 2D and 3D plots (optimized for performance)
build_hover_text <- function(df, legend_title, display_color, x_vals = NULL, y_vals = NULL, z_vals = NULL) {
    # Simplified hover text - only show most important info
    # This reduces data sent to browser significantly
    
    # Build base text
    base_text <- paste0(
        "Cell: ", df$barcode,
        "<br>", legend_title, ": ", display_color,
        "<br>Study: ", df$study_label,
        "<br>Stage: ", df$parasite_stage
    )
    
    # Add coordinates
    coords_text <- paste0("<br>X: ", round(x_vals, 2), "<br>Y: ", round(y_vals, 2))
    
    # Conditionally add Z if provided
    if (!is.null(z_vals)) {
        coords_text <- paste0(coords_text, "<br>Z: ", round(z_vals, 2))
    }
    
    return(~ paste0(base_text, coords_text))
}


# Generate 2d plot
plot_2d <- function(
    df,
    x,
    y,
    color,
    legend_title,
    size,
    is_categorical,
    selected_color,
    colors,
    is_dual_plot = FALSE) {
    display_color <- if (color == "host_species") {
        format_species_name(selected_color)
    } else {
        selected_color
    }

    hover_text <- build_hover_text(df, legend_title, display_color, x, y)

    # Calculate number of unique categories for categorical data
    num_categories <- if (is_categorical) {
        length(unique(df[[color]]))
    } else {
        NULL
     }
     
     legend_layout <- get_legend_layout(legend_title, num_categories, is_dual_plot, !is_categorical)
     plot_margins <- get_plot_margins(num_categories, is_dual_plot)
    df$plotly_key <- seq_len(nrow(df))

    if (is_categorical) {
        p <- plot_ly(
            data = df,
            x = x,
            y = y,
            type = "scattergl",  # Use WebGL for better performance with large datasets
            mode = "markers",
            color = ~ factor(df[[color]]),
            marker = list(
                size = size,
                line = list(width = 0, color = "rgba(0,0,0,0)")  # Disable borders for performance
            ),
            text = hover_text,
            hoverinfo = "text",
            colors = colors,
            width = 1000,
            height = 700,
            source = "embedding",
            key = ~plotly_key
        ) %>%
            layout(
                xaxis = list(title = paste("X-", x)),
                yaxis = list(title = paste("Y-", y)),
                legend = legend_layout,
                font = list(size = 14),
                margin = plot_margins
            )
    } else {
        p <- plot_ly(
            data = df,
            x = x,
            y = y,
            type = "scattergl",  # Use WebGL for better performance with large datasets
            mode = "markers",
            text = hover_text,
            hoverinfo = "text",
            width = 1000,
            height = 700,
            key = ~plotly_key,
            source = "embedding",
            marker = list(
                size = size,
                color = selected_color,
                colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
                showscale = TRUE,
                colorbar = legend_layout
            )
        ) %>%
            layout(
                xaxis = list(title = paste("X-", x)),
                yaxis = list(title = paste("Y-", y)),
                font = list(size = 14),
                margin = plot_margins
            )
    }
    return(p)
}


# Generate 3d plot
plot_3d <- function(
    df,
    color,
    legend_title,
    size,
    selected_color,
    is_categorical,
    colors,
    coords,
    is_dual_plot = FALSE) {
    x_col <- coords[1]
    y_col <- coords[2]
    z_col <- coords[3]

    x_vals <- df[[x_col]]
    y_vals <- df[[y_col]]
    z_vals <- df[[z_col]]

    # Format selected_color for display if it's host_species
    display_color <- if (
        !is.null(color) && !is.na(color) && color == "host_species"
    ) {
        format_species_name(selected_color)
    } else {
        selected_color
    }

    hover_text <- build_hover_text(df, legend_title, display_color, x_vals, y_vals, z_vals)

    # Calculate number of unique categories for categorical data
    num_categories <- if (is_categorical) {
        length(unique(df[[color]]))
    } else {
        NULL
     }

     legend_layout <- get_legend_layout(legend_title, num_categories, is_dual_plot, !is_categorical)
     plot_margins <- get_plot_margins(num_categories, is_dual_plot)

    if (is_categorical) {
        p <- plot_ly(
            x = x_vals,
            y = y_vals,
            z = z_vals,
            type = "scatter3d",
            mode = "markers",
            color = factor(df[[color]]),
            key = ~ df$id,
            source = "embedding",
            marker = list(size = as.numeric(size) * 0.4),
            colors = colors,
            text = hover_text,
            hoverinfo = "text",
            width = 1000,
            height = 700
        ) %>%
            layout(
                scene = list(
                    xaxis = list(title = paste("X-", x_col)),
                    yaxis = list(title = paste("Y-", y_col)),
                    zaxis = list(title = paste("Z-", z_col))
                ),
                legend = legend_layout,
                font = list(size = 14),
                margin = plot_margins
            )
    } else {
        p <- plot_ly(
            x = x_vals,
            y = y_vals,
            z = z_vals,
            type = "scatter3d",
            mode = "markers",
            marker = list(
                size = as.numeric(size) * 0.4,
                color = selected_color,
                colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
                showscale = TRUE,
                colorbar = legend_layout
            ),
            text = hover_text,
            hoverinfo = "text",
            width = 1000,
            height = 700,
            source = "embedding",
            key = ~ df$id
        ) %>%
            layout(
                scene = list(
                    xaxis = list(title = paste("X-", x_col)),
                    yaxis = list(title = paste("Y-", y_col)),
                    zaxis = list(title = paste("Z-", z_col))
                ),
                font = list(size = 14),
                margin = plot_margins
            )
    }
    return(p)
}


# Main pca, umap and tsne plotting function
embedding_plot <- function(
    df,
    xaxis_input,
    yaxis_input,
    color,
    legend_title,
    size,
    plot_type = "pca_u",
    plot_dim = "2d",
    is_dual_plot = FALSE) {
    if (is.null(color) || is.na(color) || color == "" || !color %in% names(df)) {
        color <- "sample_type"
        warning("Invalid color column, using 'sample_type' as fallback")
    }

    x <- df[[xaxis_input]]
    y <- df[[yaxis_input]]

    # ensure stable per-row idenfitifier for plotly key usage
    if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))
    if (!"plotly_key" %in% names(df)) df$plotly_key <- seq_len(nrow(df))

    selected_color <- df[[color]]
    is_categorical <- is_categorical_data(selected_color)

    if (!is.numeric(selected_color)) {
        legend_title <- color
    }

    if (is_categorical) {
        prep <- prepare_categorical_colors(df, color)
        df <- prep$df
        colors <- prep$colors
    } else {
        colors <- NULL
    }

    if (plot_dim == "2d") {
        p <- plot_2d(
            df,
            x,
            y,
            color,
            legend_title,
            size,
            is_categorical,
            selected_color,
            colors,
            is_dual_plot
        )
    } else {
        coords <- get_dim(plot_type)
        p <- plot_3d(
            df,
            color,
            legend_title,
            size,
            selected_color,
            is_categorical,
            colors,
            coords,
            is_dual_plot
        )
    }

    return(p)
}
