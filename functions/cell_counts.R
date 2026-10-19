# Counts of cells by one or two metadata fields, for the Metadata plots tab.
# =============================================================================
# count_cells() returns one row per value (or pair of values) with the number
# of cells and their percentage. Values are ordered by the biological ordering
# in config/data_columns.yml, with the missing label last. The functions here
# take physical column names and do not depend on Shiny.
# =============================================================================

# Logical keys of the fields offered in the Metadata plots tab, as select choices
# (names = display labels).
plot_field_choices <- function() {
    keys <- plot_field_keys()
    setNames(keys, vapply(keys, get_data_label, character(1)))
}

# Stop with a clear message when a column is not in the data: the data then do
# not match config/data_columns.yml, which is never caused by user input.
check_plot_columns <- function(data, columns) {
    absent <- setdiff(columns, colnames(data))
    if (length(absent) > 0) {
        stop(sprintf(
            paste0(
                "PlaViSca data error: column '%s' is not present in the ",
                "loaded data. The data do not match config/data_columns.yml; ",
                "this is not caused by user input."
            ),
            absent[[1]]
        ), call. = FALSE)
    }
    invisible(TRUE)
}

# Observed values of a column in display order: biological ordering, missing
# label last.
ordered_plot_levels <- function(values, column) {
    values <- unique(as.character(values))
    missing_label <- missing_label_for(column)
    c(
        order_column_values(column, setdiff(values, missing_label)),
        intersect(missing_label, values)
    )
}

# Values as shown in plots: configured value labels, and scientific names in
# italics (HTML) where the field asks for it.
plot_value_labels <- function(values, column) {
    if (column %in% italicised_columns()) {
        format_species_name(values, column)
    } else {
        display_values(column, values)
    }
}

# Number and percentage of cells for each value of `column1`, or for each pair
# of values of `column1` and `column2`.
#
# With one column, `percent` is the share of all counted cells; with two, it
# is the share of the cells with the same value of `column1` (each group sums
# to 100). Missing values are counted under the column's missing label, or
# left out when `show_missing` is FALSE. Both columns are returned as factors
# in display order.
count_cells <- function(data, column1, column2 = NULL, show_missing = TRUE) {
    columns <- c(column1, column2)
    check_plot_columns(data, columns)

    data <- data[, columns, drop = FALSE]
    for (col in columns) {
        if (show_missing) {
            data[[col]] <- relabel_missing(data[[col]], missing_label_for(col))
        } else {
            data <- data[!is_missing_value(data[[col]]), , drop = FALSE]
            data[[col]] <- as.character(data[[col]])
        }
    }

    counts <- dplyr::count(data, dplyr::across(dplyr::all_of(columns)), name = "cells")
    counts <- as.data.frame(counts, stringsAsFactors = FALSE)
    for (col in columns) {
        counts[[col]] <- factor(counts[[col]], levels = ordered_plot_levels(counts[[col]], col))
    }
    counts <- counts[do.call(order, unname(as.list(counts[columns]))), , drop = FALSE]
    rownames(counts) <- NULL

    group_total <- if (is.null(column2)) {
        rep(sum(counts$cells), nrow(counts))
    } else {
        stats::ave(counts$cells, counts[[column1]], FUN = sum)
    }
    counts$percent <- 100 * counts$cells / group_total
    counts
}

# The counts as a table for download: display labels as column names, values
# as shown in the app (without HTML), percentages rounded to 0.01.
count_table_for_export <- function(counts, column1, column2 = NULL) {
    columns <- c(column1, column2)
    out <- counts[, columns, drop = FALSE]
    for (col in columns) out[[col]] <- display_values(col, as.character(out[[col]]))
    names(out) <- vapply(columns, get_label_for_column, character(1))
    out$Cells <- counts$cells
    percent_name <- if (is.null(column2)) {
        "% of cells"
    } else {
        sprintf("%% of cells with the same %s", tolower(get_label_for_column(column1)))
    }
    out[[percent_name]] <- round(counts$percent, 2)
    out
}
