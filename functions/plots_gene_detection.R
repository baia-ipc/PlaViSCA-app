# Fraction of cells detecting each gene (Gene expression tab).
# =============================================================================
# gene_detection_table() counts, for each gene, the cells in which the gene is
# detected (expression > 0) for each value of a grouping field, split by a
# second field (e.g. stage and study). plot_gene_detection() draws one row per
# gene: groups on the x axis, one point per value of the second field.
# The functions here take physical column names and do not depend on Shiny.
# =============================================================================

# Plot types of a gene expression card (names = menu labels).
GENE_PLOT_TYPES <- c("Violin" = "violin", "Cells detecting" = "detection")

# Default minimum number of cells of a group and split value for a point.
DETECTION_MIN_CELLS <- 20

# One row per gene, group and split value: number of cells, number of cells
# detecting the gene and their fraction. `expression` has one column per
# gene (cells in the rows of `data`), its column names are used as gene
# labels. Cells with a missing group or split value are left out, and so are
# the combinations with fewer than `min_cells` cells. Group and split are
# factors in display order, gene a factor in the order of the columns.
gene_detection_table <- function(data, expression, group_col, split_col,
                                 min_cells = DETECTION_MIN_CELLS) {
    check_plot_columns(data, c(group_col, split_col))
    expression <- as.matrix(expression)
    stopifnot(nrow(expression) == nrow(data))

    keep <- !is_missing_value(data[[group_col]]) & !is_missing_value(data[[split_col]])
    group <- as.character(data[[group_col]])[keep]
    split <- as.character(data[[split_col]])[keep]
    expression <- expression[keep, , drop = FALSE]

    combo <- interaction(group, split, drop = TRUE, sep = "\r")
    cells <- tabulate(combo, nlevels(combo))
    parts <- strsplit(levels(combo), "\r", fixed = TRUE)

    out <- do.call(rbind, lapply(seq_len(ncol(expression)), function(j) {
        detected <- tabulate(combo[expression[, j] > 0 & !is.na(expression[, j])], nlevels(combo))
        data.frame(
            gene = colnames(expression)[[j]],
            group = vapply(parts, `[[`, character(1), 1),
            split = vapply(parts, `[[`, character(1), 2),
            cells = cells,
            detected = detected,
            stringsAsFactors = FALSE
        )
    }))
    if (is.null(out)) out <- data.frame(gene = character(0), group = character(0),
                                        split = character(0), cells = integer(0),
                                        detected = integer(0))
    out <- out[out$cells >= max(1, min_cells), , drop = FALSE]
    out$fraction <- if (nrow(out) > 0) out$detected / out$cells else numeric(0)

    out$gene <- factor(out$gene, levels = colnames(expression))
    out$group <- factor(out$group, levels = ordered_plot_levels(out$group, group_col))
    out$split <- factor(out$split, levels = ordered_plot_levels(out$split, split_col))
    out <- out[order(out$gene, out$group, out$split), , drop = FALSE]
    rownames(out) <- NULL
    out
}

# One row of points per gene: x = groups, y = % of cells detecting the gene,
# one colour per split value, the points of a group side by side.
plot_gene_detection <- function(table, group_col, split_col) {
    if (nrow(table) == 0) {
        return(message_plot("No group with enough cells to show."))
    }
    group_labels <- plot_value_labels(levels(table$group), group_col)
    split_labels <- plot_value_labels(levels(table$split), split_col)
    split_colors <- unname(category_palette(levels(table$split), split_col))
    offsets <- dodge_offsets(nlevels(table$split))
    genes <- levels(droplevels(table$gene))

    rows <- lapply(seq_along(genes), function(g) {
        part <- table[table$gene == genes[[g]], , drop = FALSE]
        p <- plot_ly()
        for (s in seq_along(split_labels)) {
            pts <- part[as.integer(part$split) == s, , drop = FALSE]
            if (nrow(pts) == 0) next
            p <- add_trace(
                p,
                x = as.integer(pts$group) + offsets[[s]],
                y = 100 * pts$fraction,
                type = "scatter",
                mode = "markers",
                name = split_labels[[s]],
                legendgroup = split_labels[[s]],
                showlegend = g == 1,
                marker = list(color = split_colors[[s]], size = 8),
                hovertext = detection_hover_text(pts, group_col, split_col,
                                                 group_labels, split_labels),
                hoverinfo = "text"
            )
        }
        layout(
            p,
            yaxis = list(title = genes[[g]], range = c(-5, 105), ticksuffix = "%",
                         tickvals = c(0, 50, 100), zeroline = FALSE),
            xaxis = list(tickvals = seq_along(group_labels), ticktext = group_labels,
                         range = c(0.5, length(group_labels) + 0.5), zeroline = FALSE)
        )
    })

    subplot(rows, nrows = length(rows), shareX = TRUE, titleY = TRUE, margin = 0.02) %>%
        layout(
            title = list(text = "Cells detecting the gene", font = list(size = 14)),
            legend = list(title = list(text = get_label_for_column(split_col)))
        )
}

# x offsets that put `n` points of a group side by side within +-0.3.
dodge_offsets <- function(n) {
    if (n <= 1) return(0)
    seq(-0.3, 0.3, length.out = n)
}

detection_hover_text <- function(pts, group_col, split_col, group_labels, split_labels) {
    sprintf(
        "<b>%s</b><br><b>%s</b>: %s<br><b>%s</b>: %s<br>Detected in %s of %s cells (%s)",
        pts$gene,
        get_label_for_column(group_col), group_labels[as.integer(pts$group)],
        get_label_for_column(split_col), split_labels[as.integer(pts$split)],
        format_cells(pts$detected), format_cells(pts$cells),
        format_percent(100 * pts$fraction)
    )
}
