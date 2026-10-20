# Colour keys of "Each gene separately" in the embedding plots: a colour grid
# for 2 genes, legend entries for 3, a colour wheel for more. The grid and
# the wheel are drawn on axes of their own (x2, y2), placed by
# gene_grid_placement() and gene_wheel_placement() (embedding_legend.R).

# Colour grid of 2 genes, placed as in `placement` (gene_grid_placement()),
# its text scaled by `text_scale`.
add_gene_colour_grid <- function(p, genes, tops, placement, text_scale = 1) {
    grid <- gene_colour_grid(genes, tops)
    n <- length(grid$colours)
    index <- matrix(seq_len(n), nrow(grid$colours))
    scale <- lapply(seq_len(n), function(i) list((i - 1) / (n - 1), grid$colours[[i]]))
    key_axis <- function(title, domain, anchor) {
        list(title = list(text = title, font = list(size = 11 * text_scale)), domain = domain, anchor = anchor,
             fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE, nticks = 3,
             tickfont = list(size = 10 * text_scale))
    }
    p <- add_trace(p, type = "heatmap", x = grid$x, y = grid$y, z = index, colorscale = scale,
                   zmin = 1, zmax = n, showscale = FALSE, text = grid$text, hoverinfo = "text",
                   xaxis = "x2", yaxis = "y2", inherit = FALSE)
    p <- layout(p,
        xaxis2 = key_axis(genes[[1]], placement$grid_x, "y2"),
        yaxis2 = key_axis(genes[[2]], placement$grid_y, "x2"),
        annotations = list(list(text = "Expression", x = placement$title_x, y = placement$title_y,
                                xref = "paper", yref = "paper", showarrow = FALSE,
                                font = list(size = 12 * text_scale)))
    )
}

# Legend of 3 genes: one entry per gene, pair and all three. Each entry is a
# tiny point on the first cell (plotly leaves traces without points out of
# the legend); the legend draws its symbols at a constant size.
# `position`: see LEGEND_POSITIONS (not "auto" or "hidden").
add_gene_colour_legend <- function(p, genes, coords, position, scale = 1) {
    entries <- gene_colour_legend(genes)
    at <- lapply(coords, function(v) v[1])
    for (i in seq_len(nrow(entries))) {
        marker <- list(color = entries$colour[[i]], size = 0.1)
        p <- if (length(coords) == 3) {
            add_trace(p, type = "scatter3d", mode = "markers", x = at[[1]], y = at[[2]], z = at[[3]],
                      name = entries$label[[i]], marker = marker, showlegend = TRUE,
                      hoverinfo = "skip", inherit = FALSE)
        } else {
            add_trace(p, type = "scattergl", mode = "markers", x = at[[1]], y = at[[2]],
                      name = entries$label[[i]], marker = marker, showlegend = TRUE,
                      hoverinfo = "skip", inherit = FALSE)
        }
    }
    legend <- category_legend_layout("Expressed genes", position, scale,
                                     if (length(coords) == 3) "3d" else "2d")
    legend$itemclick <- FALSE
    legend$itemdoubleclick <- FALSE
    layout(p, legend = legend, showlegend = TRUE)
}

# Colour wheel of the genes (gene_wheel_key()), on axes of its own placed as
# in `placement` (gene_wheel_placement()), its text scaled by `text_scale`:
# the colour disc, an outline, a spoke to each gene, the genes on the edge
# with their names, and the centre. The gene names and the title reach out of
# the axes, which do not clip them; the title follows the wheel, which plotly
# shrinks to keep it round.
add_gene_wheel_key <- function(p, genes, placement, text_scale = 1) {
    key <- gene_wheel_key(genes)
    key_axis <- function(domain, anchor) {
        list(domain = domain, anchor = anchor, range = c(-1.3, 1.3), fixedrange = TRUE,
             visible = FALSE, showgrid = FALSE, zeroline = FALSE)
    }
    pixels <- dim(key$disc)[[1]]
    step <- 2 / (pixels - 1)
    p <- add_trace(p, type = "image", z = key$disc, colormodel = "rgba", x0 = -1, dx = step,
                   y0 = -1, dy = step, hoverinfo = "skip", xaxis = "x2", yaxis = "y2", inherit = FALSE)
    outline <- seq(0, 2 * pi, length.out = 121)
    p <- add_trace(p, type = "scatter", mode = "lines", x = cos(outline), y = sin(outline),
                   line = list(color = "rgba(90,90,90,0.6)", width = 1), hoverinfo = "skip",
                   showlegend = FALSE, xaxis = "x2", yaxis = "y2", inherit = FALSE)
    p <- add_trace(p, type = "scatter", mode = "lines", x = key$spokes$x, y = key$spokes$y,
                   line = list(color = "rgba(255,255,255,0.8)", width = 1), hoverinfo = "skip",
                   showlegend = FALSE, xaxis = "x2", yaxis = "y2", inherit = FALSE)
    p <- add_trace(p, type = "scatter", mode = "markers", x = key$centre$x, y = key$centre$y,
                   marker = list(color = "white", size = 6 * text_scale,
                                 line = list(color = "#333333", width = 1.5)),
                   text = key$centre$text, hoverinfo = "text", showlegend = FALSE,
                   xaxis = "x2", yaxis = "y2", inherit = FALSE)
    p <- add_trace(p, type = "scatter", mode = "markers+text", x = key$genes$x, y = key$genes$y,
                   marker = list(color = key$genes$colour, size = 10 * text_scale,
                                 line = list(color = "white", width = 1.5)),
                   text = key$genes$label, textposition = key$genes$position,
                   textfont = list(size = 11 * text_scale), cliponaxis = FALSE,
                   hovertext = key$genes$text, hoverinfo = "text", showlegend = FALSE,
                   xaxis = "x2", yaxis = "y2", inherit = FALSE)
    y_axis <- key_axis(placement$key_y, "x2")
    y_axis$scaleanchor <- "x2"
    layout(p,
        xaxis2 = key_axis(placement$key_x, "y2"),
        yaxis2 = y_axis,
        annotations = list(list(text = "Expression", x = 0, y = 1, xref = "x2", yref = "y2",
                                yanchor = "bottom", yshift = 24 * text_scale, showarrow = FALSE,
                                font = list(size = 12 * text_scale)))
    )
}
