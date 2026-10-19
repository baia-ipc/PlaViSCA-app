# Embedding plotting helpers

y_col <- function(check_input, x_col, unintegrated, integrated) {
  if (check_input) {
    setdiff(unintegrated, x_col)
  } else {
    setdiff(integrated, x_col)
  }
}

# Assign colors based on the number of categories
assign_colors <- function(num_categories) {
  if (num_categories <= 8) {
    return(RColorBrewer::brewer.pal(
      n = max(num_categories, 3),
      name = "Set2"
    )[1:num_categories])
  } else if (num_categories <= 12) {
    return(RColorBrewer::brewer.pal(
      n = max(num_categories, 3),
      name = "Set3"
    )[1:num_categories])
  } else {
    colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(num_categories)
  }
}


# Check if the data is categorical
is_categorical_data <- function(x) {
  is.character(x) || is.factor(x)
}


# Deterministic colour vector for an ordered set of category levels.
#
# Explicit per-level colours configured in config/data_columns.yml (e.g. the
# Male/Female gametocyte pair, which must be handled symmetrically) win over
# the automatic palette; everything else is filled from the palette. The
# missing label of the column always gets the same neutral grey.
category_palette <- function(levels_vec, column = NULL) {
  levels_vec <- as.character(levels_vec)
  n <- length(levels_vec)
  if (n == 0) {
    return(setNames(character(0), character(0)))
  }

  is_missing_level <- levels_vec == missing_label_for(column)
  n_real <- sum(!is_missing_level)
  auto <- if (n_real > 0) assign_colors(n_real) else character(0)

  out <- character(n)
  out[is_missing_level] <- "#A9A9A94D"
  out[!is_missing_level] <- auto
  names(out) <- levels_vec

  if (!is.null(column)) {
    overrides <- get_column_colors(column)
    hit <- intersect(names(out), names(overrides))
    if (length(hit) > 0) out[hit] <- overrides[hit]
  }
  out
}

# Colors + biologically-ordered factor levels for a categorical colour column.
#
# True NA is shown under the missing label of the column, and values are ordered
# by the biological ordering in config/data_columns.yml rather than sort().
prepare_categorical_colors <- function(df, color) {
  # Display-only relabelling of missing values (true NA and, defensively, any
  # residual literal "NA"/"None" string). The analytical data is untouched.
  missing_label <- missing_label_for(color)
  df[[color]] <- relabel_missing(df[[color]], missing_label)

  unique_values <- unique(df[[color]])
  has_missing <- missing_label %in% unique_values
  actual_values <- setdiff(unique_values, missing_label)

  sorted_values <- order_column_values(color, actual_values)

  final_levels <- if (has_missing) {
    c(missing_label, sorted_values)
  } else {
    sorted_values
  }

  colors <- category_palette(final_levels, color)

  # legend and hover show the display names of the values (e.g. "Blood
  # schizont"); colours are configured for the values themselves
  display_levels <- display_values(color, final_levels)
  names(colors) <- display_levels
  df[[color]] <- factor(display_values(color, as.character(df[[color]])), levels = display_levels)

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


# Coordinate columns of an embedding plot: the 3 of `plot_type` in 3D; in 2D
# the chosen X and Y axes, or the first two coordinates of `plot_type` when
# the chosen ones belong to another embedding.
embedding_axes <- function(plot_type, xaxis_input, yaxis_input, plot_dim = "2d") {
  valid_coords <- get_dim(plot_type)
  if (plot_dim == "3d") {
    return(valid_coords)
  }
  if (is.null(xaxis_input) || !xaxis_input %in% valid_coords) {
    xaxis_input <- valid_coords[1]
  }
  if (is.null(yaxis_input) || !yaxis_input %in% valid_coords) {
    yaxis_input <- valid_coords[2]
  }
  c(xaxis_input, yaxis_input)
}

# Click events and mode bar of an embedding plot.
embedding_plot_config <- function(p) {
  p %>%
    event_register("plotly_click") %>%
    config(
      displaylogo = FALSE,
      scrollZoom = TRUE,
      # double-click opens the cell details (www/script.js), not a zoom reset
      doubleClick = FALSE,
      # no camera is ever saved, so "last save" would equal the default
      # a list, so that it reaches plotly.js as an array
      modeBarButtonsToRemove = list("resetCameraLastSave3d"),
      displayModeBar = TRUE,
      toImageButtonOptions = list(
        format = "png",
        filename = "embedding_plot",
        width = 1600,
        height = 1200,
        scale = 3
      )
    )
}

# Hover text for 2D and 3D plots (optimized for performance)
build_hover_text <- function(df, legend_title, display_color, x_vals = NULL, y_vals = NULL, z_vals = NULL) {
  # Simplified hover text - only show most important info
  # This reduces data sent to browser significantly

  # Build base text
  stage_col <- get_data_column("detailed_stage")
  stage_vals <- if (stage_col %in% colnames(df)) {
    relabel_missing(df[[stage_col]], missing_label_for(stage_col))
  } else {
    missing_label_for(stage_col)
  }

  base_text <- paste0(
    "Cell: ", df[[get_data_column("barcode")]],
    "<br>", legend_title, ": ", display_color,
    "<br>", get_data_label("study_label"), ": ", df[[get_data_column("study_label")]],
    "<br>", get_data_label("detailed_stage"), ": ", stage_vals
  )

  # Add coordinates only if they are numeric
  coords_text <- ""
  if (!is.null(x_vals) && is.numeric(x_vals)) {
    coords_text <- paste0("<br>X: ", round(x_vals, 2))
  }
  if (!is.null(y_vals) && is.numeric(y_vals)) {
    coords_text <- paste0(coords_text, "<br>Y: ", round(y_vals, 2))
  }

  # Conditionally add Z if provided and numeric
  if (!is.null(z_vals) && is.numeric(z_vals)) {
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
  is_dual_plot = FALSE,
  legend = legend_settings()
) {
  display_color <- if (color %in% italicised_columns()) {
    format_species_name(selected_color, color)
  } else {
    selected_color
  }

  hover_text <- build_hover_text(df, legend_title, display_color, x, y)

  key <- embedding_legend(legend_title, legend, !is_categorical, is_dual_plot, "2d")
  df$plotly_key <- seq_len(nrow(df))

  if (is_categorical) {
    p <- plot_ly(
      data = df,
      x = x,
      y = y,
      type = "scattergl", # Use WebGL for better performance with large datasets
      mode = "markers",
      color = ~ factor(df[[color]]),
      marker = list(
        size = size,
        line = list(width = 0, color = "rgba(0,0,0,0)") # Disable borders for performance
      ),
      text = hover_text,
      hoverinfo = "text",
      colors = colors,
      source = "embedding",
      key = ~plotly_key
    ) %>%
      layout(
        xaxis = list(title = paste("X-", x)),
        yaxis = list(title = paste("Y-", y)),
        legend = key$layout,
        font = list(size = 14),
        margin = key$margins,
        autosize = TRUE
      )
  } else {
    p <- plot_ly(
      data = df,
      x = x,
      y = y,
      type = "scattergl", # Use WebGL for better performance with large datasets
      mode = "markers",
      text = hover_text,
      hoverinfo = "text",
      key = ~plotly_key,
      source = "embedding",
      marker = list(
        size = size,
        color = selected_color,
        colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
        showscale = key$show,
        colorbar = key$layout
      )
    ) %>%
      layout(
        xaxis = list(title = paste("X-", x)),
        yaxis = list(title = paste("Y-", y)),
        font = list(size = 14),
        margin = key$margins,
        autosize = TRUE
      )
  }
  if (!key$show) {
    p <- layout(p, showlegend = FALSE)
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
  is_dual_plot = FALSE,
  legend = legend_settings()
) {
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
    format_species_name(selected_color, color)
  } else {
    selected_color
  }

  hover_text <- build_hover_text(df, legend_title, display_color, x_vals, y_vals, z_vals)

  # by default a 3D scene keeps all of the height: side by side too, the
  # legend goes over the plot
  key <- embedding_legend(legend_title, legend, !is_categorical, is_dual_plot, "3d")

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
      hoverinfo = "text"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste("X-", x_col)),
          yaxis = list(title = paste("Y-", y_col)),
          zaxis = list(title = paste("Z-", z_col))
        ),
        legend = key$layout,
        font = list(size = 14),
        margin = key$margins,
        autosize = TRUE
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
        showscale = key$show,
        colorbar = key$layout
      ),
      text = hover_text,
      hoverinfo = "text",
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
        margin = key$margins,
        autosize = TRUE
      )
  }
  if (!key$show) {
    p <- layout(p, showlegend = FALSE)
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
  is_dual_plot = FALSE,
  legend = legend_settings()
) {
  # A colour column that is not present means data and configuration do not
  # match (not a user-input problem): fall back to a configured field that
  # always exists, and say so loudly rather than silently plotting something
  # else.
  if (is.null(color) || is.na(color) || color == "" || !color %in% names(df)) {
    fallback <- get_data_column("broad_stage")
    if (!fallback %in% names(df)) {
      stop(sprintf(
        paste0(
          "PlaViSCA data error: neither the requested colour column ",
          "('%s') nor the configured fallback ('%s') exists in the plotting ",
          "data. Check config/data_columns.yml against the loaded dataset."
        ),
        as.character(color), fallback
      ), call. = FALSE)
    }
    warning(sprintf(
      "Invalid colour column '%s'; falling back to '%s'.",
      as.character(color), fallback
    ))
    color <- fallback
  }

  axes <- embedding_axes(plot_type, xaxis_input, yaxis_input)
  x <- df[[axes[[1]]]]
  y <- df[[axes[[2]]]]

  if (is.null(x) || is.null(y)) {
    warning("Could not resolve coordinates for plot")
    return(NULL)
  }

  # ensure stable per-row idenfitifier for plotly key usage
  if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))
  if (!"plotly_key" %in% names(df)) df$plotly_key <- seq_len(nrow(df))

  selected_color <- df[[color]]
  is_categorical <- is_categorical_data(selected_color)

  if (!is.numeric(selected_color) && (is.null(legend_title) || is.na(legend_title) || legend_title == "")) {
    legend_title <- get_label_for_column(color)
  }

  if (is_categorical) {
    prep <- prepare_categorical_colors(df, color)
    df <- prep$df
    colors <- prep$colors
    # hover text shows the missing label of the column, not NA
    selected_color <- as.character(df[[color]])
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
      is_dual_plot,
      legend
    )
    # axis titles from the column names (plot_2d() gets the coordinates)
    p <- layout(p, xaxis = list(title = paste("X-", xaxis_input)),
                yaxis = list(title = paste("Y-", yaxis_input)))
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
      is_dual_plot,
      legend
    )
  }

  return(p)
}
