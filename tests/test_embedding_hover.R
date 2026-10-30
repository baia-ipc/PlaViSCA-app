# Tests of the hover and the traces of the embedding plots
# (functions/plots/embedding_hover.R, embedding_traces() in embeddings.R):
# what is written once per trace, what a cell carries, how a point is
# mapped back to its cell, and the size of what is sent to the browser.

needs_hover_functions <- function() {
    if (!exists("hover_template") || !exists("embedding_plot")) {
        skip_test("embedding functions not loaded in this context")
    }
}

# Cells of a small UMAP (3 coordinates) of 2 studies and 3 detailed stages,
# with the expression of one gene.
hover_test_cells <- function(n = 30) {
    df <- data.frame(umap_i_1 = rnorm(n), umap_i_2 = rnorm(n), umap_i_3 = rnorm(n),
                     expression = round(rexp(n), 5), counts = sample(100:5000, n))
    df[[get_data_column("barcode")]] <- paste0("cell", seq_len(n))
    df[[get_data_column("study_label")]] <- rep(c("Study A", "Study B"), length.out = n)
    df[[get_data_column("detailed_stage")]] <- rep(c("Ring", "Trophozoite", "Schizont"), length.out = n)
    df
}

built_traces <- function(p) plotly_build(p)$x$data

no_per_cell_strings <- function(trace) {
    is.null(trace$customdata) && is.null(trace$hovertext) && is.null(trace$text) && is.null(trace$key)
}

test_that_("hover template: everything written once per trace, nothing per cell", {
    needs_hover_functions()
    study_col <- get_data_column("study_label")
    by_study <- hover_template("Study", study_col, "Study A", "Study A", "Ring")
    expect_true_(grepl("^Study: Study A<br>", by_study), "the category is written once")
    expect_true_(!grepl("Study label", by_study, fixed = TRUE), "coloured by study: the study is not repeated")
    expect_true_(grepl(": Ring<br>X: %\\{x:.2f\\}<br>Y: %\\{y:.2f\\}<extra></extra>$", by_study),
                 "the stage of the trace, then X and Y")
    expect_true_(!grepl("%{", sub("X: %\\{x:.2f\\}<br>Y: %\\{y:.2f\\}<extra></extra>$", "", by_study), fixed = TRUE),
                 "nothing per cell")
    by_stage <- hover_template("Stage", get_data_column("detailed_stage"), "Ring", "Study A", "Ring")
    expect_true_(grepl(": Study A<br>X:", by_stage, fixed = TRUE) && !grepl("Ring<br>Life", by_stage),
                 "coloured by stage: the stage is not repeated")
    numeric <- hover_template("CSP", ".gene_colour", NULL, "Study A", "Ring", n_dims = 3)
    expect_true_(grepl("CSP: %{marker.color}<br>", numeric, fixed = TRUE), "a numeric colour from the marker")
    expect_true_(grepl(": Study A<br>.*: Ring<br>", numeric), "the study and stage of the trace")
    expect_true_(grepl("<br>Z: %{z:.2f}<extra>", numeric, fixed = TRUE), "Z in 3D")
    per_cell <- hover_template("Expression", ".gene_colour", NULL, NULL, NULL, per_cell_value = TRUE)
    expect_true_(grepl("Expression: %{customdata}<br>", per_cell, fixed = TRUE), "a value string per cell")
    expect_true_(grepl(": %{hovertext}<br>", per_cell, fixed = TRUE) && grepl(": %{text}<br>", per_cell, fixed = TRUE),
                 "study and stage per cell")
})

test_that_("trace groups: one trace per category, study and stage; one legend entry per category", {
    needs_hover_functions()
    df <- hover_test_cells()
    study_col <- get_data_column("study_label")
    stage_col <- get_data_column("detailed_stage")
    groups <- embedding_trace_groups(df, study_col, TRUE)
    expect_equal_(length(groups), 6, "one trace per study and stage")
    expect_equal_(unique(vapply(groups, function(g) g$category, character(1))), c("Study A", "Study B"),
                  "the traces in the order of the levels")
    expect_equal_(vapply(groups, function(g) g$legend, logical(1)), c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
                  "the first trace of each category has the legend entry")
    expect_equal_(sort(unlist(lapply(groups, function(g) g$rows))), seq_len(nrow(df)), "every cell once")
    for (g in groups) {
        expect_true_(all(df[[study_col]][g$rows] == g$category) && all(df[[study_col]][g$rows] == g$study) &&
                     all(df[[stage_col]][g$rows] == g$stage), "the cells of a trace share its category, study and stage")
    }
    groups <- embedding_trace_groups(df, "expression", FALSE)
    expect_equal_(length(groups), 6, "a numeric colour: one trace per study and stage")
    expect_true_(all(vapply(groups, function(g) is.null(g$category) && !g$legend, logical(1))),
                 "without category or legend entry")
    groups <- embedding_trace_groups(df, ".gene_colour", FALSE, per_cell = TRUE)
    expect_equal_(length(groups), 1, "per-cell values: one trace")
    expect_true_(is.null(groups[[1]]$study) && is.null(groups[[1]]$stage), "with the study and stage per cell")
    expect_equal_(groups[[1]]$rows, seq_len(nrow(df)), "with all the cells")
})

test_that_("2D categorical plot: compact traces, the hover written once per trace", {
    needs_hover_functions()
    df <- hover_test_cells()
    study_col <- get_data_column("study_label")
    p <- embedding_plot(df, "umap_i_1", "umap_i_2", study_col, NULL, 4, plot_type = "umap_i")
    traces <- built_traces(p)
    expect_equal_(length(traces), 6, "one trace per study and stage")
    expect_equal_(sum(vapply(traces, function(t) isTRUE(t$showlegend), logical(1))), 2, "one legend entry per study")
    expect_equal_(vapply(traces, function(t) t$legendgroup, character(1)), rep(c("Study A", "Study B"), each = 3),
                  "the traces of a study in its legend group")
    first <- traces[[1]]
    expect_equal_(first$type, "scattergl", "WebGL scatter")
    expect_true_(all(vapply(traces, no_per_cell_strings, logical(1))), "nothing per cell but the coordinates")
    expect_true_(length(first$marker$color) == 1 && length(first$marker$size) == 1 &&
                 length(first$marker$line$width) == 1, "marker settings are scalars")
    rows <- which(df[[study_col]] == "Study A" & df[[get_data_column("detailed_stage")]] == "Ring")
    expect_equal_(as.numeric(first$x), round(df$umap_i_1[rows], 3), "coordinates rounded to 3 decimals")
    expect_true_(grepl(": Study A<br>Life-cycle stage: Ring<br>X:", first$hovertemplate, fixed = TRUE),
                 "the hover names the study and the stage of the trace")
    expect_equal_(attr(p, "cell_rows")[[1]], rows, "the plot knows the cells of each trace")
})

test_that_("2D numeric plot: one colour scale, colour values at 3 significant digits", {
    needs_hover_functions()
    df <- hover_test_cells()
    p <- embedding_plot(df, "umap_i_1", "umap_i_2", "expression", "CSP", 4, plot_type = "umap_i")
    built <- plotly_build(p)$x
    traces <- built$data
    expect_equal_(length(traces), 6, "one trace per study and stage")
    expect_true_(all(vapply(traces, no_per_cell_strings, logical(1))), "nothing per cell but coordinates and colour")
    expect_true_(all(vapply(traces, function(t) identical(t$marker$coloraxis, "coloraxis"), logical(1))),
                 "the traces share the colour axis")
    values <- unlist(lapply(traces, function(t) as.numeric(t$marker$color)))
    rows <- unlist(attr(p, "cell_rows"))
    expect_equal_(values, signif(df$expression[rows], 3), "expression at 3 significant digits")
    expect_equal_(built$layout$coloraxis$cmax, max(signif(df$expression, 3)), "the colour scale spans the values")
    expect_true_(grepl("CSP: %{marker.color}", traces[[1]]$hovertemplate, fixed = TRUE), "the value from the marker colour")
    expect_true_(!any(vapply(traces, function(t) isTRUE(t$showlegend), logical(1))), "no legend entries")
    counts <- built_traces(embedding_plot(df, "umap_i_1", "umap_i_2", "counts", "Counts", 4, plot_type = "umap_i"))
    expect_true_(all(as.numeric(counts[[1]]$marker$color) == round(as.numeric(counts[[1]]$marker$color))),
                 "whole numbers are kept")
})

test_that_("3D plot: scatter3d traces with rounded coordinates", {
    needs_hover_functions()
    df <- hover_test_cells()
    traces <- built_traces(embedding_plot(df, NULL, NULL, get_data_column("detailed_stage"), NULL, 4,
                                          plot_type = "umap_i", plot_dim = "3d"))
    expect_equal_(length(traces), 6, "one trace per stage and study")
    expect_equal_(sum(vapply(traces, function(t) isTRUE(t$showlegend), logical(1))), 3, "one legend entry per stage")
    t <- traces[[1]]
    rows <- df[[get_data_column("detailed_stage")]] == t$name & df[[get_data_column("study_label")]] == "Study A"
    expect_equal_(t$type, "scatter3d", "3D scatter")
    expect_equal_(as.numeric(t$z), round(df$umap_i_3[rows], 3), "Z rounded to 3 decimals")
    expect_true_(grepl("Z: %{z:.2f}", t$hovertemplate, fixed = TRUE), "Z in the hover")
    expect_true_(grepl(": Study A<br>", t$hovertemplate, fixed = TRUE), "the study of the trace")
    expect_true_(no_per_cell_strings(t), "nothing per cell")
})

test_that_("a point of a plot is mapped back to its cell", {
    needs_hover_functions()
    df <- hover_test_cells()
    p <- embedding_plot_config(embedding_plot(df, "umap_i_1", "umap_i_2", get_data_column("study_label"),
                                              NULL, 4, plot_type = "umap_i"))
    rows <- attr(p, "cell_rows")
    expect_true_(!is.null(rows), "the plot keeps the cells of its traces through its configuration")
    expect_equal_(plotted_cell(rows, 1, 2), rows[[2]][3], "trace and point counted from 0")
    expect_equal_(plotted_cell(rows, "1", "2"), rows[[2]][3], "numbers as strings")
    expect_true_(is.na(plotted_cell(rows, 6, 0)) && is.na(plotted_cell(rows, 0, 100)) &&
                 is.na(plotted_cell(rows, -1, 0)) && is.na(plotted_cell(NULL, 0, 0)) &&
                 is.na(plotted_cell(rows, NULL, 0)), "no such point")
})

test_that_("the JSON sent to the browser keeps numbers short", {
    needs_hover_functions()
    df <- hover_test_cells(5)
    df[[get_data_column("detailed_stage")]] <- c("Ring", "Ring", "Ring", "Ring", "Schizont")
    p <- embedding_plot_config(embedding_plot(df, "umap_i_1", "umap_i_2", get_data_column("detailed_stage"),
                                              NULL, 4, plot_type = "umap_i"))
    json <- attr(p$x, "TOJSON_FUNC")
    expect_true_(is.function(json), "the plot has its own serializer")
    expect_equal_(as.character(json(c(5.11, -0.123, 1))), "[5.11,-0.123,1]", "numbers with their own digits")
    text <- as.character(json(plotly_build(p)$x$data))
    expect_true_(!grepl("\"key\"", text, fixed = TRUE) && !grepl("\"customdata\"", text, fixed = TRUE) &&
                 !grepl("\"hovertext\"", text, fixed = TRUE) && !grepl("cell1", text, fixed = TRUE),
                 "no per-cell strings")
})
