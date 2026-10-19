# The embedding plot of the Embeddings tab, coloured by a category or by
# genes.

# Plot of the cells `cells` (metadata, with attr "row_idx" giving their rows
# in `expression_table`) as set by `params`, a list of
#   color   column to colour by, or "genes";
#   genes   the genes to colour by (names, or IDs if `check` is TRUE);
#   combine how several genes are combined (see gene_combine_choices());
#   xaxis, yaxis, size, plot   axes of a 2D plot, point size, "2d" or "3d".
# `plot_type` is the embedding (e.g. "umap_i"), `dual` TRUE for the two plots
# of comparison mode, `gene_names` the gene names and IDs (see genes()).
# NULL when there is nothing to draw.
embedding_figure <- function(cells, params, plot_type, dual, gene_names, expression_table) {
    if (is.null(cells) || nrow(cells) == 0) {
        return(NULL)
    }
    if (isTRUE(params$color == "genes")) {
        expression <- embedding_gene_expression(params$genes, params$check, gene_names,
                                                expression_table, attr(cells, "row_idx"))
        if (is.null(expression)) {
            return(NULL)
        }
        p <- embedding_gene_plot(cells, expression, params$combine, params$xaxis, params$yaxis,
                                 params$size, plot_type, params$plot, dual)
    } else {
        p <- embedding_plot(cells, params$xaxis, params$yaxis, params$color,
                            get_label_for_column(params$color), params$size,
                            plot_type = plot_type, plot_dim = params$plot,
                            is_dual_plot = dual)
    }
    embedding_plot_config(p)
}
