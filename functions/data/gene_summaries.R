# Expression of genes summarized per group of cells (Gene expression tab).
# =============================================================================
# gene_group_summary() gives, for each gene and each value of a grouping field
# (optionally split by a second field), the number of cells, the cells in
# which the gene is detected (expression > 0) and the mean expression. The
# Cleveland dot plot, the dot plot and the heatmap draw these summaries. The
# functions here take physical column names and do not depend on Shiny.
# =============================================================================

# Default minimum number of cells of a group for a summary.
SUMMARY_MIN_CELLS <- 20

# Values a summary plot can show (names = menu labels).
SUMMARY_VALUES <- c(
    "% detecting" = "percent",
    "Mean expression" = "mean",
    "Mean where detected" = "mean_detected"
)

# One row per gene, group (and split value): `cells`, `detected` (cells with
# expression > 0), `percent` (100 * detected / cells), `mean` (mean
# expression of all cells) and `mean_detected` (mean of the cells detecting
# the gene; NA if none). `expression` has one column per gene, one row per
# row of `data`; its column names are the gene labels. Cells with a missing
# group or split value are left out, and so are the groups with fewer than
# `min_cells` cells. Gene, group and split are factors (genes in column
# order, values in display order).
gene_group_summary <- function(data, expression, group_col, split_col = NULL,
                               min_cells = SUMMARY_MIN_CELLS) {
    columns <- c(group_col, split_col)
    check_plot_columns(data, columns)
    expression <- as.matrix(expression)
    stopifnot(nrow(expression) == nrow(data))

    keep <- Reduce(`&`, lapply(columns, function(col) !is_missing_value(data[[col]])))
    keys <- lapply(columns, function(col) as.character(data[[col]])[keep])
    expression <- expression[keep, , drop = FALSE]
    expression[is.na(expression)] <- 0

    combo <- interaction(keys, drop = TRUE, sep = "\r", lex.order = TRUE)
    n <- nlevels(combo)
    cells <- tabulate(combo, n)
    parts <- strsplit(levels(combo), "\r", fixed = TRUE)

    out <- do.call(rbind, lapply(seq_len(ncol(expression)), function(j) {
        values <- expression[, j]
        detected <- tabulate(combo[values > 0], n)
        sums <- as.vector(rowsum(values, combo, reorder = TRUE))
        row <- data.frame(
            gene = rep(colnames(expression)[[j]], n),
            group = vapply(parts, `[[`, character(1), 1),
            cells = cells,
            detected = detected,
            stringsAsFactors = FALSE
        )
        if (!is.null(split_col)) row$split <- vapply(parts, `[[`, character(1), 2)
        row$percent <- 100 * detected / cells
        row$mean <- sums / cells
        row$mean_detected <- ifelse(detected > 0, sums / detected, NA_real_)
        row
    }))
    if (is.null(out) || n == 0) {
        out <- data.frame(gene = character(0), group = character(0), cells = integer(0),
                          detected = integer(0), percent = numeric(0), mean = numeric(0),
                          mean_detected = numeric(0))
        if (!is.null(split_col)) out$split <- character(0)
    }
    out <- out[out$cells >= max(1, min_cells), , drop = FALSE]

    out$gene <- factor(out$gene, levels = colnames(expression))
    out$group <- factor(out$group, levels = ordered_plot_levels(out$group, group_col))
    sort_by <- list(out$gene, out$group)
    if (!is.null(split_col)) {
        out$split <- factor(out$split, levels = ordered_plot_levels(out$split, split_col))
        sort_by <- c(sort_by, list(out$split))
    }
    out <- out[do.call(order, sort_by), , drop = FALSE]
    rownames(out) <- NULL
    out
}

# Each gene's values scaled to mean 0 and standard deviation 1 across the
# groups, clipped to [-2.5, 2.5] (as in Seurat's dot plot). A gene with the
# same value in all groups gets 0.
scale_per_gene <- function(values, genes) {
    scaled <- stats::ave(values, genes, FUN = function(v) {
        s <- stats::sd(v, na.rm = TRUE)
        if (is.na(s) || s == 0) return(rep(0, length(v)))
        (v - mean(v, na.rm = TRUE)) / s
    })
    pmin(pmax(scaled, -2.5), 2.5)
}

# Axis or legend title of a summary value.
summary_value_title <- function(value, scaled = FALSE) {
    title <- switch(value,
        percent = "% of cells detecting",
        mean = "Mean expression",
        mean_detected = "Mean expression where detected"
    )
    if (scaled) paste(title, "<br>(scaled per gene)") else title
}

# Hover text of the rows of a summary table.
summary_hover_text <- function(table, group_col, split_col = NULL) {
    text <- sprintf(
        "<b>%s</b><br><b>%s</b>: %s",
        table$gene, get_label_for_column(group_col),
        plot_value_labels(as.character(table$group), group_col)
    )
    if (!is.null(split_col)) {
        text <- sprintf(
            "%s<br><b>%s</b>: %s", text, get_label_for_column(split_col),
            plot_value_labels(as.character(table$split), split_col)
        )
    }
    sprintf(
        "%s<br>Detected in %s of %s cells (%s)<br>Mean expression: %s<br>Mean where detected: %s",
        text, format_cells(table$detected), format_cells(table$cells),
        format_percent(table$percent), signif(table$mean, 3),
        ifelse(is.na(table$mean_detected), "-", signif(table$mean_detected, 3))
    )
}
