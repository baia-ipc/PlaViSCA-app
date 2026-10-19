# Tests of the Metadata plots tab: counting cells by one or two fields
# (functions/data/cell_counts.R) and the plot types (functions/plots/cell_counts.R).

needs_plot_functions <- function() {
    if (!exists("count_cells") || !exists("plot_cell_counts")) {
        skip_test("plot functions not loaded in this context")
    }
}

col <- function(key) get_data_column(key)

test_that_("every Metadata plots tab field is a column of the data", {
    m <- app_data("mr_data")
    absent <- Filter(function(k) !col(k) %in% colnames(m), plot_field_keys())
    expect_empty_(unlist(absent), "Metadata plots tab fields without a data column")
})

test_that_("counts by one field add up to all cells, percentages to 100", {
    needs_plot_functions()
    m <- app_data("mr_data")
    for (key in plot_field_keys()) {
        counts <- count_cells(m, col(key))
        expect_equal_(sum(counts$cells), nrow(m), sprintf("cells counted by %s", key))
        expect_equal_(sum(counts$percent), 100, sprintf("percentages by %s", key))
    }
})

test_that_("counts by two fields: percentages add up to 100 in each group", {
    needs_plot_functions()
    m <- app_data("mr_data")
    counts <- count_cells(m, col("study_label"), col("detailed_stage"))
    expect_equal_(sum(counts$cells), nrow(m), "cells counted by study and stage")
    per_study <- tapply(counts$percent, counts[[col("study_label")]], sum)
    per_study <- per_study[!is.na(per_study)]
    expect_true_(all(abs(per_study - 100) < 1e-8), "percentages of each study must add up to 100")
})

test_that_("values follow the biological ordering, missing last", {
    needs_plot_functions()
    m <- app_data("mr_data")
    column <- col("broad_stage")
    counts <- count_cells(m, column)
    observed <- as.character(counts[[column]])
    vocab <- get_field_vocabulary("broad_stage")
    expect_equal_(observed, vocab[vocab %in% observed], "broad stages in vocabulary order")

    column <- col("gametocyte_state")
    lv <- levels(count_cells(m, column)[[column]])
    expect_equal_(lv[[length(lv)]], missing_label_for(column), "missing label comes last")
})

test_that_("'Show missing' off leaves out the cells with a missing value", {
    needs_plot_functions()
    m <- app_data("mr_data")
    column <- col("gametocyte_state")
    counts <- count_cells(m, column, show_missing = FALSE)
    expect_equal_(sum(counts$cells), sum(!is_missing_value(m[[column]])), "non-missing cells")
    expect_true_(!missing_label_for(column) %in% counts[[column]], "no missing label")
})

test_that_("every plot type draws, for one and for two fields", {
    needs_plot_functions()
    m <- app_data("mr_data")
    for (type in PLOT_TYPES_ONE_FIELD) {
        for (as_percent in c(FALSE, TRUE)) {
            p <- plot_cell_counts(m, col("host_species"), NULL, type, as_percent)
            plotly::plotly_build(p)
        }
    }
    for (type in PLOT_TYPES_TWO_FIELDS) {
        for (as_percent in c(FALSE, TRUE)) {
            p <- plot_cell_counts(m, col("study_label"), col("detailed_stage"), type, as_percent)
            plotly::plotly_build(p)
        }
    }
})

test_that_("edge cases: no cells, same field twice, type for the other number of fields", {
    needs_plot_functions()
    m <- app_data("mr_data")
    plotly::plotly_build(plot_cell_counts(m[0, ], col("study_label"), col("broad_stage"), "stacked"))
    p <- plotly::plotly_build(plot_cell_counts(m, col("study_label"), col("study_label"), "heatmap"))
    expect_equal_(p$x$data[[1]]$type, "bar", "same field twice is plotted as one field")
    p <- plotly::plotly_build(plot_cell_counts(m, col("study_label"), col("broad_stage"), "pie"))
    expect_equal_(p$x$layout$barmode, "stack", "a one-field type with two fields falls back to stacked bars")
})

test_that_("the exported count table has display labels and the counts", {
    needs_plot_functions()
    m <- app_data("mr_data")
    counts <- count_cells(m, col("study_label"), col("broad_stage"))
    out <- count_table_for_export(counts, col("study_label"), col("broad_stage"))
    expect_equal_(names(out)[1:3], c(get_data_label("study_label"), get_data_label("broad_stage"), "Cells"),
        "column names of the exported table")
    expect_equal_(sum(out$Cells), nrow(m), "cells in the exported table")
})
