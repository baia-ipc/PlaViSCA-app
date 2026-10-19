# Legend of an embedding plot: where it goes, its size, and the margins it
# needs. The "Legend position" and "Legend size" plot settings choose one of
# LEGEND_POSITIONS and LEGEND_SIZES. Three kinds of legend use them: the
# legend of a category, the colour bar of gene expression, and the colour key
# of "Each gene separately" (a colour grid for 2 genes, legend entries for 3,
# a colour wheel for more).

# Over the plot in a corner, outside it (right of or below the plot), or no
# legend. "auto": below side-by-side 2D plots, which are narrow, otherwise
# over the top-left corner.
LEGEND_POSITIONS <- c(
  "Auto" = "auto", "Top left" = "top_left", "Top right" = "top_right",
  "Bottom left" = "bottom_left", "Bottom right" = "bottom_right",
  "Right" = "right", "Below" = "below", "Hidden" = "hidden"
)

# Factor applied to the text, colour bar and colour grid of the legend.
LEGEND_SIZES <- c(S = 0.8, M = 1, L = 1.25)

# Legend settings of a plot: `position` one of LEGEND_POSITIONS, `size` a name
# of LEGEND_SIZES; a missing or unknown value gives the default.
legend_settings <- function(position = "auto", size = "M") {
  if (length(position) != 1 || !position %in% LEGEND_POSITIONS) position <- "auto"
  if (length(size) != 1 || !size %in% names(LEGEND_SIZES)) size <- "M"
  list(position = position, scale = unname(LEGEND_SIZES[[size]]))
}

# The position that "auto" stands for in a plot; other positions are kept.
resolve_legend_position <- function(position, is_dual_plot = FALSE, plot_dim = "2d") {
  if (position != "auto") {
    return(position)
  }
  if (is_dual_plot && plot_dim != "3d") "below" else "top_left"
}

legend_is_outside <- function(position) position %in% c("right", "below")

# Anchor of a legend at `position` (not "auto" or "hidden"), in paper
# coordinates (0..1 across the plot area). On the right, the legend starts
# under the mode bar; below a 2D plot it leaves room for the x axis title.
legend_anchor <- function(position, plot_dim = "2d") {
  switch(position,
    top_left = list(x = 0.02, xanchor = "left", y = 0.98, yanchor = "top"),
    top_right = list(x = 0.98, xanchor = "right", y = 0.93, yanchor = "top"),
    bottom_left = list(x = 0.02, xanchor = "left", y = 0.02, yanchor = "bottom"),
    bottom_right = list(x = 0.98, xanchor = "right", y = 0.02, yanchor = "bottom"),
    right = list(x = 1.02, xanchor = "left", y = 0.93, yanchor = "top"),
    below = list(x = 0, xanchor = "left", y = if (plot_dim == "3d") 0 else -0.22, yanchor = "top"),
    stop("unknown legend position: ", position, call. = FALSE)
  )
}

# Legend of the categories (plotly `legend`) titled `title`, at `position`.
# Over the plot it has a box, so that it stays readable over the cells;
# outside, its text is smaller and below the plot it is horizontal.
category_legend_layout <- function(title, position, scale = 1, plot_dim = "2d") {
  outside <- legend_is_outside(position)
  font_sizes <- if (outside) c(title = 12, text = 11) else c(title = 14, text = 12)
  legend <- c(
    list(
      title = list(text = title, font = list(size = font_sizes[["title"]] * scale, family = "Arial")),
      orientation = if (position == "below") "h" else "v"
    ),
    legend_anchor(position, plot_dim),
    list(
      itemsizing = "constant",
      font = list(size = font_sizes[["text"]] * scale, family = "Arial"),
      itemclick = "toggle", # toggle one value on or off
      itemdoubleclick = "toggleothers" # show only this value
    )
  )
  if (outside) {
    legend$title$side <- "top"
  } else {
    legend$bgcolor <- "rgba(255,255,255,0.95)"
    legend$bordercolor <- "rgba(0,0,0,0.5)"
    legend$borderwidth <- 1
  }
  legend
}

# Colour bar of gene expression (plotly `marker.colorbar`) titled `title`, at
# `position`: half the height (or width, below the plot) of the plot area.
colorbar_layout <- function(title, position, scale = 1, plot_dim = "2d") {
  below <- position == "below"
  bar <- c(
    list(
      title = list(text = title, font = list(size = (if (below) 11 else 12) * scale, family = "Arial")),
      orientation = if (below) "h" else "v",
      len = min(0.5 * scale, 1),
      thickness = (if (below) 10 else 15) * scale,
      tickfont = list(size = (if (below) 10 else 11) * scale, family = "Arial")
    ),
    legend_anchor(position, plot_dim)
  )
  if (below) {
    bar$title$side <- "top"
  }
  if (!legend_is_outside(position)) {
    bar$bgcolor <- "rgba(255,255,255,0.8)"
    bar$borderwidth <- 0
  }
  bar
}

# Margins of a plot with its legend at `position`: as small as possible, with
# room for a legend outside the plot. The room is fixed, so that the two
# plots side by side have the same size.
legend_margins <- function(position, scale = 1) {
  list(
    l = 10,
    r = if (position == "right") round(170 * scale) else 10,
    t = 10,
    b = if (position == "below") round(180 * scale) else 10
  )
}

# Where the colour grid of 2 genes goes at `position` (not "hidden"), in paper
# coordinates: its ranges (`grid_x`, `grid_y`), the place of its title
# (`title_x`, `title_y`) and the ranges left to the cells (`plot_x`, `plot_y`).
# The grid is drawn on axes of its own, which must lie inside the figure: on
# the right of or below the plot, the cells get less room. Over the plot, the
# grid leaves room on its left and below for its axis titles. "auto" is on
# the right.
gene_grid_placement <- function(position, scale = 1) {
  w <- 0.13 * scale
  h <- 0.31 * scale
  left <- 0.09
  right <- 0.99
  top <- 0.86
  bottom <- 0.1
  place <- function(grid_x, grid_y, plot_x = c(0, 1), plot_y = c(0, 1)) {
    list(grid_x = grid_x, grid_y = grid_y, title_x = mean(grid_x), title_y = grid_y[[2]] + 0.06,
         plot_x = plot_x, plot_y = plot_y)
  }
  switch(position,
    auto = ,
    right = place(c(right - w, right), c(top - h, top), plot_x = c(0, right - w - 0.08)),
    top_left = place(c(left, left + w), c(top - h, top)),
    top_right = place(c(right - w, right), c(top - h, top)),
    bottom_left = place(c(left, left + w), c(bottom, bottom + h)),
    bottom_right = place(c(right - w, right), c(bottom, bottom + h)),
    # lower than on the side, so that the cells keep most of the height; its
    # title stays under the x axis title of the plot
    below = {
      h <- 0.22 * scale
      grid <- place(c(left, left + w), c(0.08, 0.08 + h), plot_y = c(0.08 + h + 0.2, 1))
      grid$title_y <- 0.08 + h + 0.04
      grid
    },
    stop("unknown legend position: ", position, call. = FALSE)
  )
}

# Where the colour wheel of "Each gene separately" (4 genes or more) goes at
# `position` (not "hidden"), in paper coordinates: the ranges of its axes
# (`key_x`, `key_y`) and the ranges left to the cells (`plot_x`, `plot_y`).
# The wheel is drawn on axes of its own, which must lie inside the figure: on
# the right of or below the plot, the cells get less room. The gene names
# around the wheel reach out of its axes, so the axes keep some distance from
# the edges. "auto" is on the right.
gene_wheel_placement <- function(position, scale = 1) {
  w <- 0.16 * scale
  h <- 0.26 * scale
  left <- 0.13
  right <- 0.87
  top <- 0.88
  bottom <- 0.04
  place <- function(key_x, key_y, plot_x = c(0, 1), plot_y = c(0, 1)) {
    list(key_x = key_x, key_y = key_y, plot_x = plot_x, plot_y = plot_y)
  }
  switch(position,
    auto = ,
    right = place(c(right - w, right), c(top - h, top), plot_x = c(0, right - w - 0.15)),
    top_left = place(c(left, left + w), c(top - h, top)),
    top_right = place(c(right - w, right), c(top - h, top)),
    bottom_left = place(c(left, left + w), c(bottom, bottom + h)),
    bottom_right = place(c(right - w, right), c(bottom, bottom + h)),
    # lower than on the side, so that the cells keep most of the height,
    # under the x axis title of the plot
    below = {
      h <- 0.2 * scale
      place(c(left, left + w), c(bottom, bottom + h), plot_y = c(bottom + h + 0.17, 1))
    },
    stop("unknown legend position: ", position, call. = FALSE)
  )
}

# Legend of an embedding plot, titled `title`, as set by `legend`
# (legend_settings()): the legend of the categories, or the colour bar of a
# gene plot. Returns whether it is shown (`show`), its `layout` (NULL if
# hidden) and the `margins` of the plot.
embedding_legend <- function(title, legend, is_gene_plot, is_dual_plot = FALSE, plot_dim = "2d") {
  position <- resolve_legend_position(legend$position, is_dual_plot, plot_dim)
  show <- position != "hidden"
  layout <- if (!show) {
    NULL
  } else if (is_gene_plot) {
    colorbar_layout(title, position, legend$scale, plot_dim)
  } else {
    category_legend_layout(title, position, legend$scale, plot_dim)
  }
  list(show = show, layout = layout, margins = legend_margins(position, legend$scale))
}
