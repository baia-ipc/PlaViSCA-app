# Plots of a Gene expression card: the plot types, their options and the
# plot of a card. The violin and box plots are in
# functions/plots/gene_distribution.R, the plots of summaries per group in
# functions/plots/gene_summaries.R.

# Plot types of a gene expression card (names = menu labels).
GENE_PLOT_TYPES <- c(
    "Violin plot" = "violin",
    "Box plot" = "box",
    "Cleveland dot plot" = "cleveland",
    "Dot plot (Seurat style)" = "dotplot",
    "Heatmap" = "heatmap"
)

# Options shown for each plot type (in the card, see gene_plot_options()):
# split = optional "Split by" field, combine = how several genes are
# combined, points = "Points by" field, value = summary value, scale =
# "Scale per gene".
GENE_PLOT_OPTIONS <- list(
    violin = c("split", "combine"),
    box = c("split", "combine"),
    cleveland = c("points", "value"),
    dotplot = "scale",
    heatmap = c("value", "scale")
)

# Value of the "Split by" / "Points by" menus that splits nothing.
NO_SPLIT <- "none"

# Plot of one Gene expression card. `fg` is the filtered expression data of
# the tab (data = metadata of the selected cells, full = expression of all
# cells, row_idx = rows of the selected cells in full), `genes` the selected
# gene names (or IDs when `use_ids` is TRUE). `opts` holds the options of
# the plot type: split, combine, points, value, scale; `min_cells` is the minimum
# number of cells of a group in the summary plots.
gene_expression_card_plot <- function(fg, genes, use_ids, group, type = "violin",
                                      opts = list(), min_cells = SUMMARY_MIN_CELLS) {
    if (!type %in% GENE_PLOT_TYPES) type <- "violin"
    gene_ids <- if (isTRUE(use_ids)) genes else get_gene_id(genes, fg$genes)
    found <- !is.na(gene_ids) & gene_ids %in% colnames(fg$full)
    if (!any(found)) {
        return(message_plot("Selected gene(s) not found in data"))
    }
    expression <- as.matrix(fg$full[fg$row_idx, gene_ids[found], drop = FALSE])
    colnames(expression) <- genes[found]
    split_or_null <- function(x) if (is.null(x) || !nzchar(x) || x == NO_SPLIT) NULL else x

    if (type %in% c("violin", "box")) {
        combine <- if (isTRUE(opts$combine %in% COMBINE_GENES)) opts$combine else "mean"
        return(plot_gene_distribution(fg$data, expression, group, split_or_null(opts$split),
                                      type, combine))
    }

    min_cells <- suppressWarnings(as.numeric(min_cells))
    if (length(min_cells) != 1 || is.na(min_cells)) min_cells <- SUMMARY_MIN_CELLS
    value <- if (isTRUE(opts$value %in% SUMMARY_VALUES)) opts$value else "percent"
    split <- if (type == "cleveland") split_or_null(opts$points) else NULL
    table <- gene_group_summary(fg$data, expression, group, split, min_cells)
    switch(type,
        cleveland = plot_gene_cleveland(table, group, split, value),
        dotplot = plot_gene_dotplot(table, group, scaled = !isFALSE(opts$scale)),
        heatmap = plot_gene_heatmap(table, group, value, scaled = !isFALSE(opts$scale))
    )
}

# Empty plot with a message as title (no data, or nothing selected yet).
create_empty_plot <- function(title = "") {
    plotly::plot_ly() %>%
        plotly::add_trace(x = c(), y = c(), type = "scatter", mode = "markers") %>%
        plotly::layout(
            title = title,
            xaxis = list(title = ""),
            yaxis = list(title = ""),
            showlegend = FALSE
        )
}
