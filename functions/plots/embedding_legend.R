# Legend of an embedding plot: where it goes and the margins it needs.

get_legend_layout <- function(legend_title, num_categories = NULL, is_dual_plot = FALSE, is_gene_plot = FALSE) {
  # Side by side (2D), the plots are narrow: the legend goes below the plot, in
  # the bottom margin (get_plot_margins()), so that it does not cover cells
  if (is_dual_plot && is_gene_plot) {
    return(list(
      title = list(text = legend_title, font = list(size = 11, family = "Arial"), side = "top"),
      orientation = "h",
      x = 0, xanchor = "left", y = -0.22, yanchor = "top",
      len = 0.5, thickness = 10,
      tickfont = list(size = 10, family = "Arial")
    ))
  }
  if (is_dual_plot) {
    return(list(
      title = list(text = legend_title, font = list(size = 12, family = "Arial"), side = "top"),
      orientation = "h",
      x = 0, xanchor = "left", y = -0.22, yanchor = "top",
      itemsizing = "constant",
      font = list(size = 11, family = "Arial"),
      itemclick = "toggle",
      itemdoubleclick = "toggleothers"
    ))
  }
  # Check if this is a gene plot (continuous color scale)
  if (is_gene_plot) {
    # For gene plots, use a smaller, borderless legend
    list(
      title = list(text = legend_title, font = list(size = 12, family = "Arial")),
      orientation = "v",
      x = 0.02, # Far left of plot area
      xanchor = "left",
      y = 0.98, # Top of plot area
      yanchor = "top",
      itemsizing = "constant",
      font = list(size = 11, family = "Arial"),
      bgcolor = "rgba(255,255,255,0.8)",
      borderwidth = 0, # Remove border
      len = 0.5, # Make legend half the height
      thickness = 15, # Make legend thinner
      itemclick = "toggle",
      itemdoubleclick = "toggleothers"
    )
  } else {
    # Always position legend at top-left of plot area, overlaid for consistency
    list(
      title = list(text = legend_title, font = list(size = 14, family = "Arial")),
      orientation = "v",
      x = 0.02, # Far left of plot area
      xanchor = "left",
      y = 0.98, # Top of plot area
      yanchor = "top",
      itemsizing = "constant",
      font = list(size = 12, family = "Arial"),
      bgcolor = "rgba(255,255,255,0.95)",
      bordercolor = "rgba(0,0,0,0.5)",
      borderwidth = 1,
      itemclick = "toggle", # Toggle individual items on/off
      itemdoubleclick = "toggleothers" # Double-click to show only one
    )
  }
}

# Get appropriate margins based on legend orientation
get_plot_margins <- function(num_categories = NULL, is_dual_plot = FALSE) {
  # Minimum margins to maximize plot size; side by side, the same bottom
  # margin for both plots holds the legend, so that both have the same size
  list(l = 10, r = 10, t = 10, b = if (is_dual_plot) 180 else 10)
}
