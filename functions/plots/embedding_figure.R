# The embedding plot of the Embeddings tab, coloured by a category or by
# genes.

# The settings of the plot shown when the Embeddings tab opens: the initial
# values of its controls (ui/tab_embeddings.R; `embedding` and `integrated`
# give the plot type "umap_i") and the plot drawn at startup
# (warm_embedding_plot_cache(), functions/plots/embedding_cache.R).
EMBEDDING_PLOT_DEFAULTS <- list(
    data_type = "normalize",
    embedding = "umap",
    integrated = TRUE,
    color = get_data_column("detailed_stage"),
    genes = NULL,
    check = FALSE,
    combine = "each",
    xaxis = "pc_1",
    yaxis = "pc_2",
    size = "4",
    plot = "3d",
    legend_pos = "auto",
    legend_size = "M"
)

# Plot of the cells `cells` (metadata, with attr "row_idx" giving their rows
# in the expression matrix `expression`) as set by `params`, a list of
#   color   column to colour by, or "genes";
#   genes   the genes to colour by (names, or IDs if `check` is TRUE);
#   combine how several genes are combined (see gene_combine_choices());
#   xaxis, yaxis, size, plot   axes of a 2D plot, point size, "2d" or "3d";
#   legend_pos, legend_size    legend position and size (see legend_settings()).
# `plot_type` is the embedding (e.g. "umap_i"), `dual` TRUE for the two plots
# of comparison mode, `gene_names` the gene names and IDs (see genes()).
# NULL when there is nothing to draw.
embedding_figure <- function(cells, params, plot_type, dual, gene_names, expression) {
    if (is.null(cells) || nrow(cells) == 0) {
        return(NULL)
    }
    legend <- legend_settings(params$legend_pos, params$legend_size)
    if (isTRUE(params$color == "genes")) {
        values <- embedding_gene_expression(params$genes, params$check, gene_names,
                                            expression, attr(cells, "row_idx"))
        if (is.null(values)) {
            return(NULL)
        }
        p <- embedding_gene_plot(cells, values, params$combine, params$xaxis, params$yaxis,
                                 params$size, plot_type, params$plot, dual, legend)
    } else {
        p <- embedding_plot(cells, params$xaxis, params$yaxis, params$color,
                            get_label_for_column(params$color), params$size,
                            plot_type = plot_type, plot_dim = params$plot,
                            is_dual_plot = dual, legend = legend)
    }
    embedding_plot_config(p)
}
