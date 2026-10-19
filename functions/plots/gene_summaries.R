# Plots of gene expression summarized per group (functions/data/gene_summaries.R):
# Cleveland dot plot, dot plot (Seurat style) and heatmap.

# Cleveland dot plot: one row of points per gene, groups on the x axis and
# the chosen value on the y axis. With `split_col`, one point per split
# value, side by side and coloured by it.
plot_gene_cleveland <- function(table, group_col, split_col = NULL, value = "percent") {
    table <- table[!is.na(table[[value]]), , drop = FALSE]
    if (nrow(table) == 0) {
        return(message_plot("No group with enough cells to show."))
    }
    group_labels <- plot_value_labels(levels(table$group), group_col)
    if (is.null(split_col)) {
        split_levels <- "all"
        split_labels <- "All cells"
        split_colors <- "#2C7FB8"
        split_index <- rep(1L, nrow(table))
    } else {
        split_levels <- levels(table$split)
        split_labels <- plot_value_labels(split_levels, split_col)
        split_colors <- unname(category_palette(split_levels, split_col))
        split_index <- as.integer(table$split)
    }
    offsets <- dodge_offsets(length(split_levels))
    hover <- summary_hover_text(table, group_col, split_col)
    genes <- levels(droplevels(table$gene))
    y_range <- if (value == "percent") c(-5, 105) else c(0, 1.05 * max(table[[value]]))

    rows <- lapply(seq_along(genes), function(g) {
        p <- plot_ly()
        for (s in seq_along(split_levels)) {
            rows <- table$gene == genes[[g]] & split_index == s
            if (!any(rows)) next
            p <- add_trace(
                p,
                x = as.integer(table$group[rows]) + offsets[[s]],
                y = table[[value]][rows],
                type = "scatter",
                mode = "markers",
                name = split_labels[[s]],
                legendgroup = split_labels[[s]],
                showlegend = g == 1 && !is.null(split_col),
                marker = list(color = split_colors[[s]], size = 8),
                hovertext = hover[rows],
                hoverinfo = "text"
            )
        }
        layout(
            p,
            yaxis = list(title = genes[[g]], range = y_range, zeroline = FALSE,
                         ticksuffix = if (value == "percent") "%" else ""),
            xaxis = list(tickvals = seq_along(group_labels), ticktext = group_labels,
                         range = c(0.5, length(group_labels) + 0.5), zeroline = FALSE,
                         showgrid = FALSE)
        )
    })

    p <- subplot(rows, nrows = length(rows), shareX = TRUE, titleY = TRUE, margin = 0.02) %>%
        layout(
            title = list(text = summary_value_title(value), font = list(size = 14)),
            legend = list(title = list(text = if (is.null(split_col)) "" else get_label_for_column(split_col))),
            showlegend = !is.null(split_col)
        )
    # subplot() drops shapes set on the combined plot: add them once built
    p <- plotly_build(p)
    y_axes <- c("y", paste0("y", seq_len(length(rows))[-1]))
    p$x$layout$shapes <- do.call(c, lapply(y_axes, group_separators,
                                           n = length(group_labels), y_range = y_range))
    p
}

# Grid lines between `n` groups on the x axis (at 1.5, 2.5, ...), over
# `y_range` of y axis `y_axis` and below the points.
group_separators <- function(y_axis, n, y_range) {
    lapply(seq_len(max(0, n - 1)) + 0.5, function(x) {
        list(type = "line", x0 = x, x1 = x, xref = "x", y0 = y_range[[1]], y1 = y_range[[2]],
             yref = y_axis,
             layer = "below", line = list(color = "#eeeeee", width = 1))
    })
}

# x offsets that put `n` points of a group side by side within +-0.3.
dodge_offsets <- function(n) {
    if (n <= 1) return(0)
    seq(-0.3, 0.3, length.out = n)
}

# Dot plot as in Seurat: genes x groups, the size of a dot gives the % of
# cells detecting the gene, its colour the mean expression of all cells
# (scaled per gene when `scaled`). A key of dot sizes is drawn right of the
# plot, above the colour bar.
plot_gene_dotplot <- function(table, group_col, scaled = TRUE) {
    if (nrow(table) == 0) {
        return(message_plot("No group with enough cells to show."))
    }
    group_labels <- plot_value_labels(levels(table$group), group_col)
    genes <- levels(table$gene)
    colour <- if (scaled) scale_per_gene(table$mean, table$gene) else table$mean
    key <- c(10, 25, 50, 100)

    plot_ly() %>%
        add_trace(
            x = group_labels[as.integer(table$group)],
            y = as.character(table$gene),
            type = "scatter",
            mode = "markers",
            marker = list(
                size = dot_size(table$percent), sizemode = "diameter", color = colour,
                colorscale = "Viridis",
                cmin = if (scaled) -2.5 else 0,
                cmax = if (scaled) 2.5 else max(table$mean),
                colorbar = list(title = summary_value_title("mean", scaled), len = 0.45,
                                y = 0, yanchor = "bottom", x = 1.02),
                line = list(width = 0.5, color = "#555555")
            ),
            hovertext = summary_hover_text(table, group_col),
            hoverinfo = "text"
        ) %>%
        add_trace(
            x = rep(0.2, length(key)), y = rev(seq_along(key)),
            xaxis = "x2", yaxis = "y2", type = "scatter", mode = "markers+text",
            marker = list(size = dot_size(key), color = "#999999",
                          line = list(width = 0.5, color = "#555555")),
            text = paste0(key, "%"), textposition = "middle right",
            hoverinfo = "skip", cliponaxis = FALSE
        ) %>%
        layout(
            showlegend = FALSE,
            xaxis = c(category_x_axis(get_label_for_column(group_col), group_labels),
                      list(domain = c(0, 0.78))),
            yaxis = list(title = "", categoryorder = "array", categoryarray = rev(genes)),
            xaxis2 = list(domain = c(0.84, 1), range = c(0, 1), visible = FALSE, anchor = "y2"),
            yaxis2 = list(domain = c(0.55, 0.95), range = c(0.4, length(key) + 0.6),
                          visible = FALSE, anchor = "x2"),
            annotations = list(list(
                text = "% detecting", x = 0.84, y = 1, xref = "paper", yref = "paper",
                xanchor = "left", yanchor = "top", showarrow = FALSE
            ))
        )
}

# Marker diameter (pixels) for a % of cells: the area grows with the %.
dot_size <- function(percent) 3 + 22 * sqrt(pmax(percent, 0) / 100)

# Heatmap: genes x groups, coloured by the chosen value (scaled per gene when
# `scaled`).
plot_gene_heatmap <- function(table, group_col, value = "mean", scaled = FALSE) {
    if (nrow(table) == 0) {
        return(message_plot("No group with enough cells to show."))
    }
    group_labels <- plot_value_labels(levels(table$group), group_col)
    genes <- levels(table$gene)
    z <- table[[value]]
    if (scaled) z <- scale_per_gene(z, table$gene)

    grid <- matrix(NA_real_, nrow = length(genes), ncol = length(group_labels))
    hover <- matrix("", nrow = length(genes), ncol = length(group_labels))
    idx <- cbind(as.integer(table$gene), as.integer(table$group))
    grid[idx] <- z
    hover[idx] <- summary_hover_text(table, group_col)

    plot_ly(
        x = group_labels, y = genes, z = grid, type = "heatmap",
        colorscale = "Viridis", text = hover, hoverinfo = "text",
        colorbar = list(title = summary_value_title(value, scaled)),
        xgap = 1, ygap = 1
    ) %>%
        layout(
            xaxis = category_x_axis(get_label_for_column(group_col), group_labels),
            yaxis = list(title = "", categoryorder = "array", categoryarray = rev(genes))
        )
}

category_x_axis <- function(title, labels) {
    list(title = title, categoryorder = "array", categoryarray = labels,
         type = "category", tickangle = -30)
}
