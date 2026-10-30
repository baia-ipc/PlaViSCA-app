# Embedding plots coloured by genes ("Color by" = Genes): the expression of
# the selected genes, how several genes are combined ("Combine genes", the
# choices of COMBINE_GENES in gene_distribution.R), and the plot of "Each gene
# separately", where every gene has its own colour and the colour of a cell
# mixes them: as red, green and blue for 2 or 3 genes (gene_colour_mix.R), on
# a colour wheel for more (gene_colour_wheel.R). Their keys are drawn by
# embedding_gene_keys.R.

# "Each gene separately" is offered for 2 genes up to this number: beyond it,
# neighbouring hues of the wheel are too hard to tell apart.
MAX_GENES_EACH_COLOUR <- 8

# Up to this number of genes, "Each gene separately" mixes the genes as red,
# green and blue; above it, on the colour wheel.
MAX_GENES_RGB_MIX <- 3

# Colour (RGB, 0..1) of a cell that expresses none of the genes.
GENE_COLOUR_NONE <- c(0.85, 0.85, 0.85)

# Choices of the "Combine genes" menu for `n_genes` selected genes.
gene_combine_choices <- function(n_genes) {
    if (n_genes >= 2 && n_genes <= MAX_GENES_EACH_COLOUR) {
        return(COMBINE_GENES)
    }
    COMBINE_GENES[COMBINE_GENES != "each"]
}

# The combination used for `n_genes` genes: the menu value if it is a
# choice for that number of genes, otherwise "Each gene separately" (2 to
# MAX_GENES_EACH_COLOUR genes) or the mean.
effective_gene_combine <- function(combine, n_genes) {
    choices <- gene_combine_choices(n_genes)
    if (length(combine) == 1 && combine %in% choices) {
        return(combine)
    }
    if ("each" %in% choices) "each" else "mean"
}

# Expression of the selected genes in the cells of the plot: a numeric matrix
# with one column per gene found in the expression matrix `expression`
# (named as in the menu), one row per entry of `row_idx` (all rows if NULL);
# NULL if no gene is found. `ids_entered`: the menu lists gene IDs instead of
# names.
embedding_gene_expression <- function(gene_inputs, ids_entered, gene_names, expression,
                                      row_idx = NULL) {
    gene_inputs <- as.character(gene_inputs)
    gene_inputs <- unique(gene_inputs[!is.na(gene_inputs) & nzchar(gene_inputs)])
    if (length(gene_inputs) == 0 || is.null(expression)) {
        return(NULL)
    }
    columns <- colnames(expression)
    ids <- vapply(gene_inputs, function(g) {
        if (isTRUE(ids_entered) && g %in% columns) return(g)
        id <- get_gene_id(g, gene_names)[1]
        if (!is.na(id) && id %in% columns) return(id)
        if (g %in% columns) g else NA_character_
    }, character(1))
    found <- !is.na(ids)
    if (!any(found)) {
        return(NULL)
    }
    values <- expression_values(expression, unname(ids[found]), row_idx)
    colnames(values) <- gene_inputs[found]
    values
}

# Mean or sum of the genes in each cell (one gene: its expression).
combine_gene_expression <- function(expression, combine = "mean") {
    if (ncol(expression) == 1) {
        return(expression[, 1])
    }
    if (combine == "sum") rowSums(expression, na.rm = TRUE) else rowMeans(expression, na.rm = TRUE)
}

# Expression scaled to 0..1: divided by its `upper` quantile and clipped at 1,
# so that a few very high cells do not make all others look unexpressed.
# Missing values count as 0. The value scaled to 1 is in attribute "top".
scale_gene_expression <- function(x, upper = 0.99) {
    x[is.na(x)] <- 0
    x <- pmax(x, 0)
    top <- if (length(x) > 0) unname(stats::quantile(x, upper)) else 0
    if (top <= 0) top <- max(c(x, 0))
    scaled <- if (top > 0) pmin(x / top, 1) else x * 0
    attr(scaled, "top") <- top
    scaled
}

# Colours of "Each gene separately" from the scaled expression (cells x
# genes): mixed as red, green and blue up to MAX_GENES_RGB_MIX genes, on the
# colour wheel above. Returns `colour` (hex, one per cell), `intensity` (0..1:
# the strongest gene, or on the wheel the total expression) and, on the wheel, `dominant` and `share` (see
# wheel_gene_colours()).
each_gene_colours <- function(scaled) {
    if (ncol(scaled) > MAX_GENES_RGB_MIX) {
        return(wheel_gene_colours(scaled))
    }
    list(colour = blend_gene_colours(scaled),
         intensity = scaled[cbind(seq_len(nrow(scaled)), max.col(scaled, ties.method = "first"))])
}

# Legend title of a plot coloured by `genes` combined by `combine`.
gene_colour_title <- function(genes, combine = "mean") {
    if (length(genes) == 1) {
        return(genes[[1]])
    }
    sprintf("%s of %d genes", if (combine == "sum") "Sum" else "Mean", length(genes))
}

# Embedding plot coloured by the genes of `expression` (cells x genes, rows as
# in `df`), combined by `combine` (one of COMBINE_GENES); `legend`: see
# legend_settings().
embedding_gene_plot <- function(df, expression, combine, xaxis_input, yaxis_input, size,
                                plot_type, plot_dim, is_dual_plot = FALSE, legend = legend_settings()) {
    combine <- effective_gene_combine(combine, ncol(expression))
    if (combine == "each") {
        return(embedding_each_gene_plot(df, expression, xaxis_input, yaxis_input, size, plot_type, plot_dim,
                                        is_dual_plot, legend))
    }
    df$.gene_colour <- combine_gene_expression(expression, combine)
    embedding_plot(df, xaxis_input, yaxis_input, ".gene_colour",
                   gene_colour_title(colnames(expression), combine), size,
                   plot_type = plot_type, plot_dim = plot_dim, is_dual_plot = is_dual_plot,
                   legend = legend)
}

# Embedding plot with one colour per gene (2 to MAX_GENES_EACH_COLOUR genes),
# mixed in each cell (each_gene_colours()). Cells with higher expression are
# drawn on top. The key: a colour grid for 2 genes, legend entries for 3, a
# colour wheel with the genes at their hues for more.
embedding_each_gene_plot <- function(df, expression, xaxis_input, yaxis_input, size, plot_type, plot_dim,
                                     is_dual_plot = FALSE, legend = legend_settings()) {
    position <- resolve_legend_position(legend$position, is_dual_plot, plot_dim)
    genes <- colnames(expression)
    scaled <- lapply(seq_along(genes), function(g) scale_gene_expression(expression[, g]))
    tops <- vapply(scaled, function(s) attr(s, "top"), numeric(1))
    scaled <- matrix(unlist(lapply(scaled, as.numeric)), nrow = nrow(expression))
    colours <- each_gene_colours(scaled)
    df$plotly_key <- seq_len(nrow(df))
    df$.gene_colour <- colours$colour
    df$.gene_values <- gene_hover_values(expression, colours$dominant, colours$share)

    df <- df[order(colours$intensity), , drop = FALSE]

    axes <- embedding_axes(plot_type, xaxis_input, yaxis_input, plot_dim)
    coords <- lapply(axes, function(a) df[[a]])
    hover <- build_hover_text(df, "Expression", df$.gene_values, coords[[1]], coords[[2]],
                              if (plot_dim == "3d") coords[[3]])
    p <- if (plot_dim == "3d") {
        plot_ly(x = coords[[1]], y = coords[[2]], z = coords[[3]], type = "scatter3d", mode = "markers",
                marker = list(size = as.numeric(size) * 0.4, color = df$.gene_colour),
                text = hover, hoverinfo = "text", key = df$plotly_key, source = "embedding",
                showlegend = FALSE)
    } else {
        plot_ly(data = df, x = coords[[1]], y = coords[[2]], type = "scattergl", mode = "markers",
                marker = list(size = size, color = df$.gene_colour, line = list(width = 0)),
                text = hover, hoverinfo = "text", key = ~plotly_key, source = "embedding",
                showlegend = FALSE)
    }
    if (position == "hidden") {
        p <- layout(p, font = list(size = 14), margin = legend_margins(position), autosize = TRUE)
        return(embedding_axis_titles(p, axes, plot_dim))
    }
    if (length(genes) == 3) {
        p <- layout(p, font = list(size = 14), margin = legend_margins(position, legend$scale), autosize = TRUE)
        p <- embedding_axis_titles(p, axes, plot_dim)
        return(add_gene_colour_legend(p, genes, coords, position, legend$scale))
    }
    # the colour grid and the colour wheel lie inside the figure, also on the
    # right or below; by default on the right, and then, side by side, with
    # the margins of the other plot, so that both have the same size
    auto <- legend$position == "auto"
    side <- if (auto) "right" else position
    placement <- if (length(genes) == 2) gene_grid_placement(side, legend$scale) else
        gene_wheel_placement(side, legend$scale)
    margins <- legend_margins(if (auto && is_dual_plot) position else "hidden", legend$scale)
    p <- layout(p, font = list(size = 14), margin = margins, autosize = TRUE)
    p <- embedding_axis_titles(p, axes, plot_dim, placement$plot_x, placement$plot_y)
    if (length(genes) == 2) {
        return(add_gene_colour_grid(p, genes, tops, placement, legend$scale))
    }
    add_gene_wheel_key(p, genes, placement, legend$scale)
}

# Hover text of each cell of an "Each gene separately" plot: the expression
# of each gene and, if `dominant` is given (the colour wheel), the gene with
# the largest share of the cells that express any.
gene_hover_values <- function(expression, dominant = NULL, share = NULL) {
    genes <- colnames(expression)
    values <- lapply(seq_along(genes), function(g) sprintf("%s %.2f", genes[[g]], expression[, g]))
    text <- do.call(paste, c(values, sep = ", "))
    if (is.null(dominant)) {
        return(text)
    }
    expressed <- !is.na(dominant)
    text[expressed] <- sprintf("%s<br>Dominant: %s (%.0f%%)", text[expressed],
                               genes[dominant[expressed]], 100 * share[expressed])
    text
}

# Axis titles of an embedding plot (2D axes or 3D scene); `x_domain`,
# `y_domain`: the part of the width and height left for the plot.
embedding_axis_titles <- function(p, axes, plot_dim, x_domain = c(0, 1), y_domain = c(0, 1)) {
    if (plot_dim == "3d") {
        return(layout(p, scene = list(
            domain = list(x = x_domain, y = y_domain),
            xaxis = list(title = paste("X-", axes[[1]])),
            yaxis = list(title = paste("Y-", axes[[2]])),
            zaxis = list(title = paste("Z-", axes[[3]]))
        )))
    }
    layout(p, xaxis = list(title = paste("X-", axes[[1]]), domain = x_domain),
           yaxis = list(title = paste("Y-", axes[[2]]), domain = y_domain))
}
