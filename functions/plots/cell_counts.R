# Plots of cell counts by one or two metadata fields (Metadata plots tab).
# =============================================================================
# plot_cell_counts() draws a count table from count_cells() as one of the plot
# types below. One field: each value is coloured as in the other tabs. Two
# fields: the first field groups the cells (bars, heatmap rows, inner ring,
# one pie each) and the second field gives the colours.
# =============================================================================

# Plot types for one field and for two fields (names = menu labels).
PLOT_TYPES_ONE_FIELD <- c("Bar chart" = "bar", "Pie chart" = "pie")
PLOT_TYPES_TWO_FIELDS <- c(
    "Stacked bars" = "stacked",
    "Grouped bars" = "grouped",
    "Heatmap" = "heatmap",
    "Sunburst" = "sunburst",
    "One pie per group" = "pies"
)

# "One pie per group" draws at most this many pies.
MAX_PIE_GROUPS <- 12

# All plot types, grouped by the number of fields (menu of the plot builder).
PLOT_TYPE_CHOICES <- list(
    "One category" = PLOT_TYPES_ONE_FIELD,
    "Two categories" = PLOT_TYPES_TWO_FIELDS
)

plot_types_for <- function(two_fields) {
    if (two_fields) PLOT_TYPES_TWO_FIELDS else PLOT_TYPES_ONE_FIELD
}

# TRUE if the plot type counts the cells by two fields.
is_two_field_type <- function(type) {
    isTRUE(type %in% PLOT_TYPES_TWO_FIELDS)
}

# Plot of the selected cells counted by `column1` (and `column2`). `type` is
# one of the plot types above; a type that does not fit the number of fields
# falls back to the first one that does. `as_percent` shows percentages
# instead of numbers of cells (see count_cells() for what they are relative
# to).
plot_cell_counts <- function(data, column1, column2 = NULL, type = NULL,
                             as_percent = FALSE, show_missing = TRUE) {
    if (!is.null(column2) && (!nzchar(column2) || identical(column2, column1))) {
        column2 <- NULL
    }
    types <- plot_types_for(!is.null(column2))
    if (is.null(type) || !type %in% types) type <- types[[1]]

    counts <- count_cells(data, column1, column2, show_missing = show_missing)
    if (nrow(counts) == 0) {
        return(message_plot("No cells to show with the selected filters."))
    }

    switch(type,
        bar = plot_counts_bar(counts, column1, as_percent),
        pie = plot_counts_pie(counts, column1, as_percent),
        stacked = plot_counts_bars_by(counts, column1, column2, as_percent, "stack"),
        grouped = plot_counts_bars_by(counts, column1, column2, as_percent, "group"),
        heatmap = plot_counts_heatmap(counts, column1, column2, as_percent),
        sunburst = plot_counts_sunburst(counts, column1, column2, as_percent),
        pies = plot_counts_pies(counts, column1, column2, as_percent)
    )
}

# An empty plot with a message in the middle.
message_plot <- function(text) {
    plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(text = text, showarrow = FALSE, x = 0.5, y = 0.5,
                        xref = "paper", yref = "paper") %>%
        layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

# ------------------------------------------------------------------- helpers

format_cells <- function(n) format(n, big.mark = ",", trim = TRUE)
format_percent <- function(p) sprintf("%.1f%%", p)

# Hover text of each row of a count table.
count_hover_text <- function(counts, column1, column2 = NULL) {
    first <- sprintf(
        "<b>%s</b>: %s", get_label_for_column(column1),
        plot_value_labels(as.character(counts[[column1]]), column1)
    )
    if (is.null(column2)) {
        return(sprintf(
            "%s<br>Cells: %s (%s)", first,
            format_cells(counts$cells), format_percent(counts$percent)
        ))
    }
    sprintf(
        "%s<br><b>%s</b>: %s<br>Cells: %s<br>%s of the cells with this %s",
        first, get_label_for_column(column2),
        plot_value_labels(as.character(counts[[column2]]), column2),
        format_cells(counts$cells), format_percent(counts$percent),
        tolower(get_label_for_column(column1))
    )
}

count_values <- function(counts, as_percent) {
    if (as_percent) counts$percent else counts$cells
}

value_axis_title <- function(as_percent, column1 = NULL) {
    if (!as_percent) {
        return("Cells")
    }
    if (is.null(column1)) {
        return("% of cells")
    }
    sprintf("%% of cells with the same %s", tolower(get_label_for_column(column1)))
}

# Display labels of the levels of a count table column, and its colours by
# level.
level_labels <- function(counts, column) {
    plot_value_labels(levels(counts[[column]]), column)
}
level_colors <- function(counts, column) {
    unname(category_palette(levels(counts[[column]]), column))
}

# Category axis listing the values top-down in display order.
category_axis <- function(title, labels) {
    list(
        title = title, categoryorder = "array", categoryarray = labels,
        autorange = "reversed", ticklen = 5, tickcolor = "transparent"
    )
}

pie_text_info <- function(as_percent) if (as_percent) "percent" else "value"

# ---------------------------------------------------------------- one field

plot_counts_bar <- function(counts, column1, as_percent) {
    labels <- level_labels(counts, column1)
    idx <- as.integer(counts[[column1]])
    plot_ly(
        x = count_values(counts, as_percent),
        y = labels[idx],
        type = "bar",
        orientation = "h",
        marker = list(color = level_colors(counts, column1)[idx]),
        hovertext = count_hover_text(counts, column1),
        hoverinfo = "text",
        textposition = "none"
    ) %>%
        layout(
            xaxis = list(title = value_axis_title(as_percent)),
            yaxis = category_axis(get_label_for_column(column1), labels),
            showlegend = FALSE,
            bargap = 0.1
        )
}

plot_counts_pie <- function(counts, column1, as_percent) {
    idx <- as.integer(counts[[column1]])
    plot_ly(
        labels = level_labels(counts, column1)[idx],
        values = counts$cells,
        type = "pie",
        sort = FALSE,
        direction = "clockwise",
        marker = list(colors = level_colors(counts, column1)[idx]),
        textinfo = pie_text_info(as_percent),
        textposition = "inside",
        hovertext = count_hover_text(counts, column1),
        hoverinfo = "text"
    ) %>%
        layout(
            legend = list(title = list(text = get_label_for_column(column1))),
            uniformtext = list(minsize = 10, mode = "hide")
        )
}

# --------------------------------------------------------------- two fields

# Horizontal bars, one per value of `column1`, split by `column2` into stacked
# (barmode "stack") or side-by-side ("group") pieces.
plot_counts_bars_by <- function(counts, column1, column2, as_percent, barmode) {
    labels1 <- level_labels(counts, column1)
    labels2 <- level_labels(counts, column2)
    colors2 <- level_colors(counts, column2)
    hover <- count_hover_text(counts, column1, column2)

    p <- plot_ly()
    for (i in seq_along(labels2)) {
        rows <- as.integer(counts[[column2]]) == i
        if (!any(rows)) next
        part <- counts[rows, , drop = FALSE]
        p <- add_trace(
            p,
            x = count_values(part, as_percent),
            y = labels1[as.integer(part[[column1]])],
            name = labels2[[i]],
            type = "bar",
            orientation = "h",
            marker = list(color = colors2[[i]]),
            hovertext = hover[rows],
            hoverinfo = "text",
            textposition = "none"
        )
    }
    p %>%
        layout(
            barmode = barmode,
            xaxis = list(title = value_axis_title(as_percent, column1)),
            yaxis = category_axis(get_label_for_column(column1), labels1),
            legend = list(title = list(text = get_label_for_column(column2)),
                          traceorder = "normal"),
            bargap = 0.15
        )
}

# One row per value of `column1`, one column per value of `column2`; each cell
# holds the number (or percentage) of cells, written on it. Pairs without
# cells are left blank.
plot_counts_heatmap <- function(counts, column1, column2, as_percent) {
    labels1 <- level_labels(counts, column1)
    labels2 <- level_labels(counts, column2)
    i <- as.integer(counts[[column1]])
    j <- as.integer(counts[[column2]])

    z <- matrix(NA_real_, length(labels1), length(labels2))
    shown <- matrix("", length(labels1), length(labels2))
    hover <- matrix("", length(labels1), length(labels2))
    z[cbind(i, j)] <- count_values(counts, as_percent)
    shown[cbind(i, j)] <- if (as_percent) {
        sprintf("%.1f", counts$percent)
    } else {
        format_cells(counts$cells)
    }
    hover[cbind(i, j)] <- count_hover_text(counts, column1, column2)

    plot_ly(
        x = labels2,
        y = labels1,
        z = z,
        type = "heatmap",
        colorscale = list(c(0, "#deebf7"), c(1, "#08306b")),
        text = shown,
        texttemplate = "%{text}",
        hovertext = hover,
        hoverinfo = "text",
        hoverongaps = FALSE,
        xgap = 1,
        ygap = 1,
        colorbar = list(title = list(text = value_axis_title(as_percent)))
    ) %>%
        layout(
            xaxis = list(title = get_label_for_column(column2), type = "category",
                         categoryorder = "array", categoryarray = labels2),
            yaxis = category_axis(get_label_for_column(column1), labels1)
        )
}

# Inner ring: values of `column1`; outer ring: values of `column2` within
# each of them. Ring sizes are numbers of cells; the labels show numbers or
# percentages of the ring below.
plot_counts_sunburst <- function(counts, column1, column2, as_percent) {
    inner <- aggregate(cells ~ group, data.frame(group = counts[[column1]], cells = counts$cells),
                                          FUN = sum)
    inner_labels <- level_labels(counts, column1)[as.integer(inner$group)]
    inner_ids <- paste0("g", as.integer(inner$group))
    inner_hover <- sprintf(
        "<b>%s</b>: %s<br>Cells: %s (%s)", get_label_for_column(column1), inner_labels,
        format_cells(inner$cells), format_percent(100 * inner$cells / sum(inner$cells))
    )

    outer_ids <- paste0("g", as.integer(counts[[column1]]), "_", as.integer(counts[[column2]]))
    outer_labels <- level_labels(counts, column2)[as.integer(counts[[column2]])]

    plot_ly(
        ids = c(inner_ids, outer_ids),
        labels = c(inner_labels, outer_labels),
        parents = c(rep("", length(inner_ids)), paste0("g", as.integer(counts[[column1]]))),
        values = c(inner$cells, counts$cells),
        type = "sunburst",
        branchvalues = "total",
        sort = FALSE,
        marker = list(colors = c(
            level_colors(counts, column1)[as.integer(inner$group)],
            level_colors(counts, column2)[as.integer(counts[[column2]])]
        )),
        textinfo = if (as_percent) "label+percent parent" else "label+value",
        hovertext = c(inner_hover, count_hover_text(counts, column1, column2)),
        hoverinfo = "text"
    ) %>%
        layout(uniformtext = list(minsize = 10, mode = "hide"))
}

# One pie for each value of `column1`, its slices the values of `column2`.
plot_counts_pies <- function(counts, column1, column2, as_percent) {
    labels1 <- level_labels(counts, column1)
    labels2 <- level_labels(counts, column2)
    colors2 <- level_colors(counts, column2)
    groups <- sort(unique(as.integer(counts[[column1]])))
    if (length(groups) > MAX_PIE_GROUPS) {
        return(message_plot(sprintf(
            paste0(
                "%d groups are too many for one pie each (at most %d).<br>",
                "Select fewer with the filters, or choose another plot type."
            ),
            length(groups), MAX_PIE_GROUPS
        )))
    }

    n_columns <- min(length(groups), 4)
    n_rows <- ceiling(length(groups) / n_columns)
    hover <- count_hover_text(counts, column1, column2)

    # Every pie lists all values of column2, absent ones with 0 cells, so that
    # the shared legend keeps the display order.
    p <- plot_ly()
    for (k in seq_along(groups)) {
        rows <- which(as.integer(counts[[column1]]) == groups[[k]])
        j <- as.integer(counts[[column2]][rows])
        cells <- numeric(length(labels2))
        cells[j] <- counts$cells[rows]
        slice_hover <- rep("", length(labels2))
        slice_hover[j] <- hover[rows]
        p <- add_trace(
            p,
            labels = labels2,
            values = cells,
            type = "pie",
            sort = FALSE,
            direction = "clockwise",
            marker = list(colors = colors2),
            textinfo = pie_text_info(as_percent),
            textposition = "inside",
            hovertext = slice_hover,
            hoverinfo = "text",
            title = list(text = labels1[[groups[[k]]]], position = "top center"),
            domain = list(row = (k - 1) %/% n_columns, column = (k - 1) %% n_columns)
        )
    }
    p %>%
        layout(
            grid = list(rows = n_rows, columns = n_columns),
            uniformtext = list(minsize = 10, mode = "hide"),
            legend = list(title = list(text = get_label_for_column(column2)))
        )
}
