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
        "genes" = get_gene_id(gene_input, gene_data_obj),
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
    df[[color]][is.na(df[[color]])] <- "NA (missing)"

    unique_values <- unique(df[[color]])

    has_na <- "NA (missing)" %in% unique_values
    if (has_na) {
        actual_values <- unique_values[unique_values != "NA (missing)"]
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
        c("NA (missing)", sorted_values)
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


get_legend_layout <- function(legend_title) {
    list(
        title = list(text = legend_title),
        orientation = "v",
        x = 1.02,
        xanchor = "left",
        y = 1,
        yanchor = "top",
        len = 0.5
    )
}


# Hover text for 2D and 3D plots
build_hover_text <- function(df, legend_title, display_color, x_vals = NULL, y_vals = NULL, z_vals = NULL) {
    # Conditionally include Z label/value only when provided
    z_label <- if (is.null(z_vals)) "" else "<br>Z:"
    z_value <- if (is.null(z_vals)) "" else z_vals

    return(~ paste(
        "Cell_id:",
        df$barcode,
        "<br>Color by",
        legend_title,
        ":",
        display_color,
        "<br>Cluster:",
        df$seurat_clusters,
        "<br>Study:",
        df$study_label,
        "<br>Life cycle stage:",
        df$life_cycle_stage,
        "<br>Host species:",
        format_species_name(df$host_species),
        "<br>Sample type:",
        df$sample_type,
        "<br>Sequencer:",
        df$sequencer,
        "<br>Strain:",
        df$strain,
        "<br>n_Features_RNA:",
        df$n_feature_rna,
        "<br>X:",
        x_vals,
        "<br>Y:",
        y_vals,
        z_label,
        z_value
    ))
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
    colors) {
    display_color <- if (color == "host_species") {
        format_species_name(selected_color)
    } else {
        selected_color
    }

    hover_text <- build_hover_text(df, legend_title, display_color, x, y)

    legend_layout <- get_legend_layout(legend_title)
    df$plotly_key <- seq_len(nrow(df))

    if (is_categorical) {
        p <- plot_ly(
            data = df,
            x = x,
            y = y,
            type = "scatter",
            mode = "markers",
            color = ~ factor(df[[color]]),
            marker = list(size = size),
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
                margin = list(l = 50, r = 50, t = 25, b = 50)
            )
    } else {
        p <- plot_ly(
            data = df,
            x = x,
            y = y,
            type = "scatter",
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
                margin = list(l = 50, r = 50, t = 25, b = 50)
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
    coords) {
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

    legend_layout <- get_legend_layout(legend_title)

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
                margin = list(l = 50, r = 50, t = 25, b = 50)
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
                margin = list(l = 50, r = 50, t = 25, b = 50)
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
    plot_dim = "2d") {
    if (is.null(color) || is.na(color) || color == "" || !color %in% names(df)) {
        color <- "sample_type"
        warning("Invalid color column, using 'sample_type' as fallback")
    }

    x <- df[[xaxis_input]]
    y <- df[[yaxis_input]]
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
            colors
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
            coords
        )
    }

    return(p)
}
