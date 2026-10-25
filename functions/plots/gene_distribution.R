# Violin and box plots of the expression of each cell (Gene expression tab).

# How the expression of several genes is combined in a cell (names = menu
# labels). On log-normalized data the mean is the log of the geometric mean
# of expression + 1.
COMBINE_GENES <- c("Mean" = "mean", "Sum" = "sum", "Each gene separately" = "each")

# Violin or box plot of the expression of each cell by `group_col`
# (optionally split by `split_col`, side by side and coloured by it). Several
# genes are combined by `combine`: their mean or sum in each cell, or one row
# of plots per gene ("each"). Cells with a missing group or split value are
# left out. At most 5,000 cells are drawn (sampled within each group); the
# statistics in the hover text use all cells.
plot_gene_distribution <- function(data, expression, group_col, split_col = NULL,
                                   kind = "violin", combine = "mean") {
    check_plot_columns(data, c(group_col, split_col))
    genes <- colnames(expression)
    if (length(genes) == 1) {
        return(distribution_plot(data, expression[, 1], group_col, split_col, kind, genes[[1]]))
    }
    if (combine == "each") {
        rows <- lapply(seq_along(genes), function(g) {
            distribution_plot(data, expression[, g], group_col, split_col, kind,
                              y_title = genes[[g]], show_legend = g == 1)
        })
        return(subplot(rows, nrows = length(rows), shareX = TRUE, titleY = TRUE, margin = 0.02))
    }
    values <- if (combine == "sum") rowSums(expression, na.rm = TRUE) else rowMeans(expression, na.rm = TRUE)
    title <- sprintf("%s of %d genes: %s", if (combine == "sum") "Sum" else "Mean",
                     length(genes), paste(genes, collapse = ", "))
    distribution_plot(data, values, group_col, split_col, kind, title)
}

# Violin or box plot of `values` (one per cell of `data`).
distribution_plot <- function(data, values, group_col, split_col, kind, title = NULL,
                              y_title = "Expression", show_legend = TRUE) {
    columns <- c(group_col, split_col)
    df <- data.frame(expr = values)
    for (col in columns) df[[col]] <- as.character(data[[col]])
    keep <- !is.na(df$expr)
    for (col in columns) keep <- keep & !is_missing_value(df[[col]])
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0) {
        return(message_plot("No cells to show with the selected filters."))
    }

    group_levels <- ordered_plot_levels(df[[group_col]], group_col)
    group_labels <- plot_value_labels(group_levels, group_col)
    df$x <- group_labels[match(df[[group_col]], group_levels)]
    colour_col <- if (is.null(split_col)) group_col else split_col
    colour_levels <- ordered_plot_levels(df[[colour_col]], colour_col)
    colour_labels <- plot_value_labels(colour_levels, colour_col)
    colours <- unname(category_palette(colour_levels, colour_col))

    stats <- stats::aggregate(expr ~ ., df[, c("expr", columns), drop = FALSE],
                              function(v) c(mean = mean(v), sd = stats::sd(v), n = length(v)))
    df <- sample_cells_per_group(df, columns, 5000)
    key <- function(d) do.call(paste, c(unname(as.list(d[columns])), sep = "\r"))
    st <- stats$expr[match(key(df), key(stats)), , drop = FALSE]
    hover <- sprintf(
        "%s<br>Expression: %s<br>Mean of the group: %s<br>SD: %s<br>Cells in the group: %s",
        if (is.null(split_col)) df$x else paste0(df$x, "<br>", plot_value_labels(df[[split_col]], split_col)),
        signif(df$expr, 3), signif(st[, "mean"], 3), signif(st[, "sd"], 3), format_cells(st[, "n"])
    )

    # Without a split, one trace per group, drawn over each other. With a
    # split, one trace per group and split value, side by side: the traces
    # of a group take the slots 1, 2, ... in the order of the split values
    # present in it (offsetgroup), so that a group with fewer split values
    # than the plot does not keep empty slots, and the legend has one entry
    # per split value.
    p <- plot_ly()
    legend_done <- character(0)
    style <- if (kind == "violin") {
        list(box = list(visible = TRUE), meanline = list(visible = TRUE),
             points = if (is.null(split_col)) "all" else FALSE, jitter = 0.3,
             pointpos = 0, scalemode = "width", spanmode = "hard", bandwidth = 0.2)
    } else {
        list(boxpoints = FALSE, boxmean = TRUE)
    }
    add_distribution <- function(p, rows, i, slot = NULL) {
        label <- colour_labels[[i]]
        trace <- list(
            p, x = df$x[rows], y = df$expr[rows], type = kind,
            name = label, legendgroup = label,
            line = list(color = colours[[i]]), fillcolor = colours[[i]],
            marker = list(color = colours[[i]], size = 2, opacity = 0.5),
            text = hover[rows], hoverinfo = "text",
            showlegend = show_legend && !is.null(split_col) && !label %in% legend_done
        )
        if (!is.null(slot)) trace <- c(trace, list(alignmentgroup = "groups", offsetgroup = as.character(slot)))
        legend_done <<- union(legend_done, label)
        do.call(add_trace, c(trace, style))
    }
    for (i in seq_along(colour_levels)) {
        rows <- df[[colour_col]] == colour_levels[[i]]
        if (!any(rows)) next
        if (is.null(split_col)) {
            p <- add_distribution(p, rows, i)
            next
        }
        for (group in group_labels) {
            group_rows <- df$x == group
            present <- colour_levels[colour_levels %in% df[[colour_col]][group_rows]]
            if (!colour_levels[[i]] %in% present) next
            p <- add_distribution(p, rows & group_rows, i, slot = match(colour_levels[[i]], present))
        }
    }
    mode <- if (is.null(split_col)) "overlay" else "group"
    p <- layout(
        p,
        boxmode = if (kind == "box") mode,
        title = if (!is.null(title)) list(text = title, font = list(size = 14)),
        xaxis = category_x_axis("", group_labels),
        yaxis = list(title = y_title),
        legend = list(title = list(text = if (is.null(split_col)) "" else get_label_for_column(split_col)))
    )
    # plotly.js knows violinmode and the gaps between the plots, but the
    # attribute check of the R package warns about them: set violinmode only
    # where it is needed, and the gaps (smaller than the default 0.3, so that
    # the plots are wider) in the browser, once the plot is drawn
    if (kind == "violin" && !is.null(split_col)) p <- layout(p, violinmode = mode)
    gaps <- if (kind == "violin") "{violingap: 0.15, violingroupgap: 0.1}" else "{boxgap: 0.15, boxgroupgap: 0.1}"
    p <- htmlwidgets::onRender(p, sprintf("function(el) { Plotly.relayout(el, %s); }", gaps))
    p
}

# At most `max_cells` rows, sampled within each combination of `columns` in
# proportion to its size (reproducibly).
sample_cells_per_group <- function(df, columns, max_cells) {
    if (nrow(df) <= max_cells) return(df)
    keys <- interaction(df[columns], drop = TRUE)
    set.seed(42)
    keep <- unlist(lapply(split(seq_len(nrow(df)), keys), function(rows) {
        rows[sample.int(length(rows), max(1, round(length(rows) * max_cells / nrow(df))))]
    }), use.names = FALSE)
    df[sort(keep), , drop = FALSE]
}
