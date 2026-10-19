# Embedding plots coloured by genes ("Color by" = Genes): the expression of
# the selected genes, how several genes are combined ("Combine genes", the
# choices of COMBINE_GENES in plots_gene_distribution.R), and the colours of
# "Each gene separately", where every gene has its own colour and the colour
# of a cell mixes them.

# "Each gene separately" is offered for 2 genes up to this number.
MAX_GENES_EACH_COLOUR <- 3

# Colour (RGB, 0..1) of a cell that expresses none of the genes, and the
# colour channel of each gene: gene 1 red, gene 2 green, gene 3 blue.
GENE_BLEND_NONE <- c(0.85, 0.85, 0.85)
GENE_BLEND_CHANNELS <- diag(3)

# Brightness of the mixed colour when 1, 2 or 3 genes are fully expressed:
# lower for several genes, so that their mixtures (yellow, cyan, magenta)
# stay readable on white and the three genes together give a dark grey.
GENE_BLEND_BRIGHTNESS <- c(0.9, 0.8, 0.25)

# Choices of the "Combine genes" menu for `n_genes` selected genes.
gene_combine_choices <- function(n_genes) {
    if (n_genes >= 2 && n_genes <= MAX_GENES_EACH_COLOUR) {
        return(COMBINE_GENES)
    }
    COMBINE_GENES[COMBINE_GENES != "each"]
}

# The combination used for `n_genes` genes: the menu value if it is a
# choice for that number of genes, otherwise the mean.
effective_gene_combine <- function(combine, n_genes) {
    if (length(combine) == 1 && combine %in% gene_combine_choices(n_genes)) combine else "mean"
}

# Expression of the selected genes in the cells of the plot: a numeric matrix
# with one column per gene found in `expression_table` (named as in the
# menu), one row per entry of `row_idx` (all rows if NULL); NULL if no gene is
# found. `ids_entered`: the menu lists gene IDs instead of names.
embedding_gene_expression <- function(gene_inputs, ids_entered, gene_names, expression_table,
                                      row_idx = NULL) {
    gene_inputs <- as.character(gene_inputs)
    gene_inputs <- unique(gene_inputs[!is.na(gene_inputs) & nzchar(gene_inputs)])
    if (length(gene_inputs) == 0 || is.null(expression_table)) {
        return(NULL)
    }
    columns <- colnames(expression_table)
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
    rows <- if (is.null(row_idx)) seq_len(nrow(expression_table)) else row_idx
    expression <- vapply(ids[found], function(id) as.numeric(expression_table[[id]][rows]),
                         numeric(length(rows)))
    expression <- matrix(expression, nrow = length(rows))
    colnames(expression) <- gene_inputs[found]
    expression
}

# Mean or sum of the genes in each cell (one gene: its expression).
combine_gene_expression <- function(expression, combine = "mean") {
    if (ncol(expression) == 1) {
        return(expression[, 1])
    }
    if (combine == "sum") rowSums(expression, na.rm = TRUE) else rowMeans(expression, na.rm = TRUE)
}

# Legend title of a plot coloured by `genes` combined by `combine`.
gene_colour_title <- function(genes, combine = "mean") {
    if (length(genes) == 1) {
        return(genes[[1]])
    }
    sprintf("%s of %d genes", if (combine == "sum") "Sum" else "Mean", length(genes))
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

# Colour (hex) of each row of `scaled` (cells x 1 to 3 genes, values 0..1).
# The strongest gene sets the intensity (light grey at 0, the full colour at
# 1) and the ratios of the genes set the hue: red and green mix to yellow,
# red and blue to magenta, green and blue to cyan, all three to dark grey.
blend_gene_colours <- function(scaled) {
    scaled <- as.matrix(scaled)
    k <- ncol(scaled)
    if (k < 1 || k > MAX_GENES_EACH_COLOUR) {
        stop("blend_gene_colours(): 1 to ", MAX_GENES_EACH_COLOUR, " genes, got ", k, call. = FALSE)
    }
    strength <- apply(scaled, 1, max)
    hue <- scaled / ifelse(strength > 0, strength, 1)
    brightness <- stats::approx(seq_along(GENE_BLEND_BRIGHTNESS), GENE_BLEND_BRIGHTNESS,
                                xout = pmax(1, rowSums(hue)), rule = 2)$y
    full <- (hue %*% GENE_BLEND_CHANNELS[seq_len(k), , drop = FALSE]) * brightness
    none <- matrix(GENE_BLEND_NONE, nrow(scaled), 3, byrow = TRUE)
    mixed <- none * (1 - strength) + full * strength
    grDevices::rgb(mixed[, 1], mixed[, 2], mixed[, 3])
}

# Colour key of 2 genes: a grid of expression of gene 1 (x) by gene 2 (y),
# from 0 to the value shown at full colour (`tops`). Returns the x and y
# values, the colours of the grid (rows = y) and its hover text.
gene_colour_grid <- function(genes, tops, steps = 9) {
    levels <- seq(0, 1, length.out = steps)
    grid <- expand.grid(g1 = levels, g2 = levels)
    colours <- matrix(blend_gene_colours(as.matrix(grid)), steps, steps, byrow = TRUE)
    text <- matrix(sprintf("%s: %.2f<br>%s: %.2f", genes[[1]], grid$g1 * tops[[1]],
                           genes[[2]], grid$g2 * tops[[2]]), steps, steps, byrow = TRUE)
    list(x = levels * tops[[1]], y = levels * tops[[2]], colours = colours, text = text)
}

# Legend entries of 3 genes: each gene alone, each pair, all three, none.
gene_colour_legend <- function(genes) {
    sets <- c(lapply(seq_along(genes), function(i) i), utils::combn(seq_along(genes), 2, simplify = FALSE),
              list(seq_along(genes)))
    labels <- vapply(sets, function(s) paste(genes[s], collapse = " + "), character(1))
    labels[length(labels)] <- "All three"
    colours <- vapply(sets, function(s) {
        x <- numeric(length(genes))
        x[s] <- 1
        blend_gene_colours(matrix(x, nrow = 1))
    }, character(1))
    data.frame(label = c(labels, "None"),
               colour = c(colours, blend_gene_colours(matrix(0, 1, length(genes)))),
               stringsAsFactors = FALSE)
}

# Embedding plot coloured by the genes of `expression` (cells x genes, rows as
# in `df`), combined by `combine` (one of COMBINE_GENES).
embedding_gene_plot <- function(df, expression, combine, xaxis_input, yaxis_input, size,
                                plot_type, plot_dim, is_dual_plot = FALSE) {
    combine <- effective_gene_combine(combine, ncol(expression))
    if (combine == "each") {
        return(embedding_blend_plot(df, expression, xaxis_input, yaxis_input, size, plot_type, plot_dim))
    }
    df$.gene_colour <- combine_gene_expression(expression, combine)
    embedding_plot(df, xaxis_input, yaxis_input, ".gene_colour",
                   gene_colour_title(colnames(expression), combine), size,
                   plot_type = plot_type, plot_dim = plot_dim, is_dual_plot = is_dual_plot)
}

# Embedding plot with one colour per gene (2 or 3 genes), mixed in each cell
# (blend_gene_colours()). Cells with higher expression are drawn on top. The
# key: a colour grid for 2 genes, legend entries for 3.
embedding_blend_plot <- function(df, expression, xaxis_input, yaxis_input, size, plot_type, plot_dim) {
    genes <- colnames(expression)
    scaled <- lapply(seq_along(genes), function(g) scale_gene_expression(expression[, g]))
    tops <- vapply(scaled, function(s) attr(s, "top"), numeric(1))
    scaled <- do.call(cbind, lapply(scaled, as.numeric))
    df$plotly_key <- seq_len(nrow(df))
    df$.gene_colour <- blend_gene_colours(scaled)
    values <- vapply(seq_along(genes), function(g) {
        sprintf("%s %.2f", genes[[g]], expression[, g])
    }, character(nrow(df)))
    values <- matrix(values, nrow = nrow(df))
    df$.gene_values <- apply(values, 1, paste, collapse = ", ")

    drawn <- order(apply(scaled, 1, max))
    df <- df[drawn, , drop = FALSE]

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
    p <- layout(p, font = list(size = 14), margin = get_plot_margins(), autosize = TRUE)
    # the colour grid of 2 genes sits right of the plot
    p <- embedding_axis_titles(p, axes, plot_dim, domain = if (length(genes) == 2) c(0, 0.78) else c(0, 1))
    if (length(genes) == 2) add_gene_colour_grid(p, genes, tops) else add_gene_colour_legend(p, genes, coords)
}

# Axis titles of an embedding plot (2D axes or 3D scene); `domain`: the part
# of the width left for the plot.
embedding_axis_titles <- function(p, axes, plot_dim, domain = c(0, 1)) {
    if (plot_dim == "3d") {
        return(layout(p, scene = list(
            domain = list(x = domain, y = c(0, 1)),
            xaxis = list(title = paste("X-", axes[[1]])),
            yaxis = list(title = paste("Y-", axes[[2]])),
            zaxis = list(title = paste("Z-", axes[[3]]))
        )))
    }
    layout(p, xaxis = list(title = paste("X-", axes[[1]]), domain = domain),
           yaxis = list(title = paste("Y-", axes[[2]])))
}

# Colour grid of 2 genes, in the right part of the plot area.
add_gene_colour_grid <- function(p, genes, tops) {
    grid <- gene_colour_grid(genes, tops)
    n <- length(grid$colours)
    index <- matrix(seq_len(n), nrow(grid$colours))
    scale <- lapply(seq_len(n), function(i) list((i - 1) / (n - 1), grid$colours[[i]]))
    key_axis <- function(title, domain, anchor) {
        list(title = list(text = title, font = list(size = 11)), domain = domain, anchor = anchor,
             fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE, nticks = 3,
             tickfont = list(size = 10))
    }
    p <- add_trace(p, type = "heatmap", x = grid$x, y = grid$y, z = index, colorscale = scale,
                   zmin = 1, zmax = n, showscale = FALSE, text = grid$text, hoverinfo = "text",
                   xaxis = "x2", yaxis = "y2", inherit = FALSE)
    p <- layout(p,
        xaxis2 = key_axis(genes[[1]], c(0.86, 0.99), "y2"),
        yaxis2 = key_axis(genes[[2]], c(0.55, 0.86), "x2"),
        annotations = list(list(text = "Expression", x = 0.925, y = 0.92, xref = "paper",
                                yref = "paper", showarrow = FALSE, font = list(size = 12)))
    )
}

# Legend of 3 genes: one entry per gene, pair and all three. Each entry is a
# tiny point on the first cell (plotly leaves traces without points out of
# the legend); the legend draws its symbols at a constant size.
add_gene_colour_legend <- function(p, genes, coords) {
    entries <- gene_colour_legend(genes)
    at <- lapply(coords, function(v) v[1])
    for (i in seq_len(nrow(entries))) {
        marker <- list(color = entries$colour[[i]], size = 0.1)
        p <- if (length(coords) == 3) {
            add_trace(p, type = "scatter3d", mode = "markers", x = at[[1]], y = at[[2]], z = at[[3]],
                      name = entries$label[[i]], marker = marker, showlegend = TRUE,
                      hoverinfo = "skip", inherit = FALSE)
        } else {
            add_trace(p, type = "scattergl", mode = "markers", x = at[[1]], y = at[[2]],
                      name = entries$label[[i]], marker = marker, showlegend = TRUE,
                      hoverinfo = "skip", inherit = FALSE)
        }
    }
    legend <- get_legend_layout("Expressed genes")
    legend$itemclick <- FALSE
    legend$itemdoubleclick <- FALSE
    layout(p, legend = legend, showlegend = TRUE)
}
