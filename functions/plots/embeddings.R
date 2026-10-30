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

# Click events, mode bar and JSON of an embedding plot.
embedding_plot_config <- function(p) {
  p %>%
    with_compact_json() %>%
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

# The marker traces of the cells `df`, added to the plot `p`: one trace per
# colour category (if `color` is categorical: named by it, in the order of
# the levels, in the colours `colors`), study and detailed stage, so that
# the hover is written once per trace (embedding_trace_groups(),
# embedding_hover.R). A numeric `color` colours the cells by the values
# `selected_color` on one colour scale shared by the traces (plotly
# `coloraxis`), with the colour bar `key` (embedding_legend()). The
# coordinates are the columns .x, .y and, in 3D (`n_dims` 3), .z of `df`;
# `marker` are the marker settings shared by the traces. A trace keeps only
# the columns it draws: plotly stores the data frame it is given inside the
# plot, which is cached.
embedding_traces <- function(p, df, n_dims, color, legend_title, is_categorical, selected_color,
                             colors, marker, key) {
  coords <- c(".x", ".y", ".z")[seq_len(n_dims)]
  add_cells <- function(p, rows, columns, marker, name = NULL, legend = FALSE) {
    args <- list(p, data = df[rows, columns, drop = FALSE], x = ~.x, y = ~.y, type = "scattergl",
                 mode = "markers", marker = marker, showlegend = legend)
    if (n_dims == 3) {
      args$z <- ~.z
      args$type <- "scatter3d"
    }
    if (!is.null(name)) {
      args$name <- name
    }
    do.call(add_trace, args)
  }

  groups <- embedding_trace_groups(df, color, is_categorical)
  if (is_categorical) {
    for (group in groups) {
      p <- add_cells(p, group$rows, coords, c(marker, list(color = toRGB(colors[[group$category]]))),
                     group$category, group$legend)
    }
    categories <- vapply(groups, function(group) group$category, character(1))
    if (color %in% italicised_columns()) {
      categories <- format_species_name(categories, color)
    }
    return(add_embedding_hover(p, df, color, legend_title, groups, categories, n_dims))
  }

  df$.colour <- compact_colour_values(selected_color)
  for (group in groups) {
    p <- add_cells(p, group$rows, c(coords, ".colour"), c(marker, list(color = ~.colour, coloraxis = "coloraxis")))
  }
  p <- layout(p, coloraxis = colour_scale_layout(df$.colour, key))
  add_embedding_hover(p, df, color, legend_title, groups, NULL, n_dims)
}

# The colour scale (plotly `layout.coloraxis`) of the numeric `values` of
# the cells, blue to red over their range, with the colour bar `key`
# (embedding_legend()).
colour_scale_layout <- function(values, key) {
  finite <- values[is.finite(values)]
  range <- if (length(finite) > 0) range(finite) else c(0, 1)
  if (range[1] == range[2]) range[2] <- range[1] + 1
  list(
    colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
    cmin = range[1], cmax = range[2],
    showscale = key$show,
    colorbar = key$layout
  )
}


# 2D plot of the cells `df` at the coordinates `x` and `y`, coloured by the
# column `color` (categorical, in the colours `colors`, or numeric with the
# values `selected_color`).
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
  key <- embedding_legend(legend_title, legend, !is_categorical, is_dual_plot, "2d")
  df$.x <- compact_coordinates(x)
  df$.y <- compact_coordinates(y)

  p <- embedding_traces(
    plot_ly(source = "embedding"), df, 2, color, legend_title, is_categorical, selected_color,
    colors, marker = list(size = size, line = list(width = 0)), key
  )
  p <- layout(
    p,
    font = list(size = 14),
    margin = key$margins,
    autosize = TRUE
  )
  if (is_categorical) {
    p <- layout(p, legend = key$layout)
  }
  if (!key$show) {
    p <- layout(p, showlegend = FALSE)
  }
  return(p)
}


# 3D plot of the cells `df` at the coordinate columns `coords`, coloured
# like plot_2d().
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
  # by default a 3D scene keeps all of the height: side by side too, the
  # legend goes over the plot
  key <- embedding_legend(legend_title, legend, !is_categorical, is_dual_plot, "3d")
  df$.x <- compact_coordinates(df[[coords[1]]])
  df$.y <- compact_coordinates(df[[coords[2]]])
  df$.z <- compact_coordinates(df[[coords[3]]])

  p <- embedding_traces(
    plot_ly(source = "embedding"), df, 3, color, legend_title, is_categorical, selected_color,
    colors, marker = list(size = as.numeric(size) * 0.4), key
  )
  p <- layout(
    p,
    scene = list(
      xaxis = list(title = paste("X-", coords[1])),
      yaxis = list(title = paste("Y-", coords[2])),
      zaxis = list(title = paste("Z-", coords[3]))
    ),
    font = list(size = 14),
    margin = key$margins,
    autosize = TRUE
  )
  if (is_categorical) {
    p <- layout(p, legend = key$layout)
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

  selected_color <- df[[color]]
  is_categorical <- is_categorical_data(selected_color)

  if (!is.numeric(selected_color) && (is.null(legend_title) || is.na(legend_title) || legend_title == "")) {
    legend_title <- get_label_for_column(color)
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
