# Tests of the hover and the traces of the embedding plots
# (functions/plots/embedding_hover.R, embedding_traces() in embeddings.R):
# what a cell carries, what is written once per trace, and the size of what
# is sent to the browser.

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

test_that_("hover template: per-trace values written once, per-cell values as plotly variables", {
    needs_hover_functions()
    study_col <- get_data_column("study_label")
    by_study <- hover_template("Study", study_col, "Study A", "Ring")
    expect_true_(grepl("Cell: %{customdata}", by_study, fixed = TRUE), "the barcode is the customdata")
    expect_true_(grepl("Study: Study A<br>", by_study, fixed = TRUE), "the category is written once")
    expect_true_(grepl(": Study A<br>.*: Ring<br>X: %\\{x:.2f\\}<br>Y: %\\{y:.2f\\}<extra></extra>$", by_study),
                 "study and stage of the trace, then X and Y")
    expect_true_(!grepl("%{hovertext}", by_study, fixed = TRUE) && !grepl("%{text}", by_study, fixed = TRUE),
                 "nothing per cell but the barcode")
    numeric <- hover_template("CSP", ".gene_colour", NULL, NULL, n_dims = 3)
    expect_true_(grepl("CSP: %{marker.color}<br>", numeric, fixed = TRUE), "a numeric colour from the marker")
    expect_true_(grepl(": %{hovertext}<br>", numeric, fixed = TRUE), "the study of each cell")
    expect_true_(grepl(": %{text}<br>", numeric, fixed = TRUE), "the stage of each cell")
    expect_true_(grepl("<br>Z: %{z:.2f}<extra>", numeric, fixed = TRUE), "Z in 3D")
})

test_that_("trace groups: one trace per category and stage up to the limit, then per category", {
    needs_hover_functions()
    df <- hover_test_cells()
    study_col <- get_data_column("study_label")
    groups <- embedding_trace_groups(df, study_col, TRUE)
    expect_equal_(length(groups), 6, "2 studies x 3 stages")
    expect_equal_(vapply(groups, function(g) g$category, character(1)),
                  rep(c("Study A", "Study B"), each = 3), "the traces of a category follow each other")
    expect_equal_(vapply(groups, function(g) g$legend, logical(1)),
                  rep(c(TRUE, FALSE, FALSE), 2), "one legend entry per category")
    expect_equal_(sort(unlist(lapply(groups, function(g) g$rows))), seq_len(nrow(df)), "every cell once")
    for (g in groups) {
        expect_true_(all(df[[study_col]][g$rows] == g$category) &&
                     all(df[[get_data_column("detailed_stage")]][g$rows] == g$stage),
                     "the cells of a trace share its category and stage")
    }
    df$many <- paste0("c", seq_len(nrow(df)))
    groups <- embedding_trace_groups(df, "many", TRUE)
    expect_equal_(length(groups), nrow(df), "beyond the limit: one trace per category")
    expect_true_(all(vapply(groups, function(g) is.null(g$stage), logical(1))), "the stage is then per cell")
    groups <- embedding_trace_groups(df, "expression", FALSE)
    expect_equal_(length(groups), 1, "a numeric colour: one trace")
    expect_equal_(groups[[1]]$rows, seq_len(nrow(df)), "with all the cells")
})

test_that_("2D categorical plot: compact traces with the hover of each cell", {
    needs_hover_functions()
    df <- hover_test_cells()
    study_col <- get_data_column("study_label")
    traces <- built_traces(embedding_plot(df, "umap_i_1", "umap_i_2", study_col, NULL, 4, plot_type = "umap_i"))
    expect_equal_(length(traces), 6, "one trace per study and stage")
    expect_equal_(sum(vapply(traces, function(t) isTRUE(t$showlegend), logical(1))), 2, "one legend entry per study")
    expect_equal_(unique(vapply(traces, function(t) t$legendgroup, character(1))), c("Study A", "Study B"),
                  "the traces of a study share its legend group")
    first <- traces[[1]]
    expect_equal_(first$type, "scattergl", "WebGL scatter")
    expect_true_(is.null(first$key) && is.null(first$text) && is.null(first$hovertext),
                 "nothing per cell but coordinates and barcode")
    expect_true_(length(first$marker$color) == 1 && length(first$marker$size) == 1 &&
                 length(first$marker$line$width) == 1, "marker settings are scalars")
    rows <- which(df[[study_col]] == "Study A" & df[[get_data_column("detailed_stage")]] == "Ring")
    expect_equal_(as.character(first$customdata), paste0("cell", rows), "the barcodes of the cells of the trace")
    expect_equal_(as.numeric(first$x), round(df$umap_i_1[rows], 3), "coordinates rounded to 3 decimals")
    expect_true_(grepl("Study label: Study A<br>Study label: Study A<br>Life-cycle stage: Ring<br>X:",
                       first$hovertemplate, fixed = TRUE) ||
                 grepl(": Study A<br>.*: Ring<br>X:", first$hovertemplate),
                 "the hover names the study and the stage of the trace")
})

test_that_("2D numeric plot: colour values at 3 significant digits, stage and study per cell", {
    needs_hover_functions()
    df <- hover_test_cells()
    traces <- built_traces(embedding_plot(df, "umap_i_1", "umap_i_2", "expression", "CSP", 4, plot_type = "umap_i"))
    expect_equal_(length(traces), 1, "one trace")
    t <- traces[[1]]
    expect_equal_(as.numeric(t$marker$color), signif(df$expression, 3), "expression at 3 significant digits")
    expect_equal_(as.character(t$text), df[[get_data_column("detailed_stage")]], "the stage of each cell")
    expect_equal_(as.character(t$hovertext), df[[get_data_column("study_label")]], "the study of each cell")
    expect_true_(grepl("CSP: %{marker.color}", t$hovertemplate, fixed = TRUE), "the value from the marker colour")
    expect_true_(is.null(t$key), "no key per cell")
    counts <- built_traces(embedding_plot(df, "umap_i_1", "umap_i_2", "counts", "Counts", 4, plot_type = "umap_i"))
    expect_equal_(as.numeric(counts[[1]]$marker$color), df$counts, "whole numbers are kept")
})

test_that_("3D plot: scatter3d traces with rounded coordinates and the barcode of each cell", {
    needs_hover_functions()
    df <- hover_test_cells()
    traces <- built_traces(embedding_plot(df, NULL, NULL, get_data_column("detailed_stage"), NULL, 4,
                                          plot_type = "umap_i", plot_dim = "3d"))
    expect_equal_(length(traces), 3, "one trace per stage")
    t <- traces[[1]]
    rows <- df[[get_data_column("detailed_stage")]] == t$name
    expect_equal_(t$type, "scatter3d", "3D scatter")
    expect_equal_(as.numeric(t$z), round(df$umap_i_3[rows], 3), "Z rounded to 3 decimals")
    expect_true_(grepl("Z: %{z:.2f}", t$hovertemplate, fixed = TRUE), "Z in the hover")
    expect_true_(grepl(": %{hovertext}<br>", t$hovertemplate, fixed = TRUE), "the study of each cell")
    expect_equal_(as.character(t$customdata), paste0("cell", which(rows)), "the barcodes of its cells")
})

test_that_("the JSON sent to the browser keeps numbers short and single cells as arrays", {
    needs_hover_functions()
    df <- hover_test_cells(5)
    df[[get_data_column("detailed_stage")]] <- c("Ring", "Ring", "Ring", "Ring", "Schizont")
    p <- embedding_plot_config(embedding_plot(df, "umap_i_1", "umap_i_2", get_data_column("detailed_stage"),
                                              NULL, 4, plot_type = "umap_i"))
    json <- attr(p$x, "TOJSON_FUNC")
    expect_true_(is.function(json), "the plot has its own serializer")
    expect_equal_(as.character(json(c(5.11, -0.123, 1))), "[5.11,-0.123,1]", "numbers with their own digits")
    text <- as.character(json(plotly_build(p)$x$data))
    expect_true_(grepl("\"customdata\":[\"cell5\"]", text, fixed = TRUE), "one cell is still an array")
    expect_true_(!grepl("\"key\"", text, fixed = TRUE), "no key array")
})
