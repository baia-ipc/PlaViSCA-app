# Tests of the legend of the embedding plots (functions/plots/embedding_legend.R):
# "Legend position" and "Legend size" for the legend of a category, the
# colour bar of gene expression and the colour keys of "Each gene separately".

needs_legend_functions <- function() {
    if (!exists("legend_settings") || !exists("embedding_gene_plot")) {
        skip_test("embedding functions not loaded in this context")
    }
}

# Cells of a small 3-coordinate UMAP with a category column.
legend_test_cells <- function(n = 30) {
    df <- data.frame(umap_i_1 = rnorm(n), umap_i_2 = rnorm(n), umap_i_3 = rnorm(n),
                     barcode = paste0("cell", seq_len(n)))
    names(df)[names(df) == "barcode"] <- get_data_column("barcode")
    df[[get_data_column("study_label")]] <- rep(c("Study A", "Study B"), length.out = n)
    df[[get_data_column("detailed_stage")]] <- "Ring"
    df
}

test_that_("legend settings: defaults for missing or unknown values", {
    needs_legend_functions()
    expect_equal_(legend_settings(), list(position = "auto", scale = 1), "Auto, M by default")
    expect_equal_(legend_settings(NULL, NULL), legend_settings(), "NULL inputs give the defaults")
    expect_equal_(legend_settings("nowhere", "XL"), legend_settings(), "unknown values give the defaults")
    expect_equal_(legend_settings("right", "L")$scale, LEGEND_SIZES[["L"]], "size L")
})

test_that_("legend position Auto: below side-by-side 2D plots, otherwise top left", {
    needs_legend_functions()
    expect_equal_(resolve_legend_position("auto", FALSE, "2d"), "top_left", "single 2D plot")
    expect_equal_(resolve_legend_position("auto", TRUE, "2d"), "below", "side-by-side 2D plots")
    expect_equal_(resolve_legend_position("auto", TRUE, "3d"), "top_left", "side-by-side 3D plots")
    expect_equal_(resolve_legend_position("right", TRUE, "2d"), "right", "a chosen position is kept")
})

test_that_("legend position Auto, size M: the legends over and below the plot", {
    needs_legend_functions()
    over <- category_legend_layout("Stage", "top_left")
    expect_equal_(c(over$x, over$y), c(0.02, 0.98), "category legend at the top left")
    expect_equal_(c(over$title$font$size, over$font$size), c(14, 12), "category legend text over the plot")
    expect_equal_(over$borderwidth, 1, "category legend over the plot has a box")
    below <- category_legend_layout("Stage", "below")
    expect_equal_(c(below$orientation, below$y), c("h", "-0.22"), "category legend below a 2D plot")
    expect_equal_(c(below$title$font$size, below$font$size), c(12, 11), "category legend text below the plot")
    expect_true_(is.null(below$bgcolor), "no box below the plot")
    bar <- colorbar_layout("CSP", "below")
    expect_equal_(c(bar$len, bar$thickness, bar$title$font$size), c(0.5, 10, 11), "colour bar below the plot")
    expect_equal_(legend_margins("below"), list(l = 10, r = 10, t = 10, b = 180), "room below the plot")
    expect_equal_(legend_margins("top_left"), list(l = 10, r = 10, t = 10, b = 10), "minimal margins")
    grid <- gene_grid_placement("auto")
    expect_equal_(c(grid$grid_x, grid$grid_y, grid$plot_x), c(0.86, 0.99, 0.55, 0.86, 0, 0.78),
                  "colour grid of 2 genes on the right")
    key <- gene_wheel_placement("auto")
    expect_equal_(c(key$key_x, key$key_y, key$plot_x), c(0.71, 0.87, 0.62, 0.88, 0, 0.56),
                  "colour wheel on the right")
})

test_that_("legend positions and sizes: anchors, margins, colour grids and wheels", {
    needs_legend_functions()
    inside <- c("top_left", "top_right", "bottom_left", "bottom_right")
    for (position in setdiff(LEGEND_POSITIONS, c("auto", "hidden"))) {
        for (size in names(LEGEND_SIZES)) {
            scale <- LEGEND_SIZES[[size]]
            label <- sprintf("%s %s", position, size)
            legend <- category_legend_layout("Stage", position, scale)
            expect_equal_(legend$font$size, (if (position %in% inside) 12 else 11) * scale,
                          paste(label, "legend text scaled"))
            bar <- colorbar_layout("CSP", position, scale)
            expect_true_(bar$len <= 1, paste(label, "colour bar within the plot"))
            margins <- legend_margins(position, scale)
            expect_true_((margins$r > 10) == (position == "right"), paste(label, "room on the right"))
            expect_true_((margins$b > 10) == (position == "below"), paste(label, "room below"))
            grid <- gene_grid_placement(position, scale)
            ranges <- c(grid$grid_x, grid$grid_y, grid$plot_x, grid$plot_y, grid$title_y)
            expect_true_(all(ranges >= 0 & ranges <= 1), paste(label, "colour grid inside the figure"))
            key <- gene_wheel_placement(position, scale)
            ranges <- c(key$key_x, key$key_y, key$plot_x, key$plot_y)
            expect_true_(all(ranges >= 0 & ranges <= 1), paste(label, "colour wheel inside the figure"))
            if (!position %in% inside) {
                apart <- grid$grid_x[[1]] > grid$plot_x[[2]] || grid$grid_y[[2]] < grid$plot_y[[1]]
                expect_true_(apart, paste(label, "colour grid beside the cells"))
                apart <- key$key_x[[1]] > key$plot_x[[2]] || key$key_y[[2]] < key$plot_y[[1]]
                expect_true_(apart, paste(label, "colour wheel beside the cells"))
            }
        }
    }
})

test_that_("embedding plots: every legend position and size, all kinds of legend", {
    needs_legend_functions()
    df <- legend_test_cells()
    n <- nrow(df)
    expression <- cbind(CSP = runif(n), AMA1 = runif(n), SBP1 = runif(n), DBP = runif(n))
    for (dim in c("2d", "3d")) {
        for (dual in c(FALSE, TRUE)) {
            for (position in LEGEND_POSITIONS) {
                legend <- legend_settings(position, "L")
                label <- sprintf("%s %s%s", position, dim, if (dual) " side by side" else "")
                p <- embedding_plot(df, "umap_i_1", "umap_i_2", get_data_column("study_label"), NULL, 4,
                                    plot_type = "umap_i", plot_dim = dim, is_dual_plot = dual, legend = legend)
                built <- plotly_build(p)$x
                expect_equal_(isFALSE(built$layout$showlegend), position == "hidden",
                              paste(label, "category legend hidden only if asked"))
                p <- embedding_gene_plot(df, expression[, 1, drop = FALSE], "mean", "umap_i_1", "umap_i_2", 4,
                                         "umap_i", dim, dual, legend)
                built <- plotly_build(p)$x
                expect_equal_(isTRUE(built$layout$coloraxis$showscale), position != "hidden",
                              paste(label, "colour bar shown unless hidden"))
                for (genes in list(1:2, 1:3, 1:4)) {
                    p <- embedding_gene_plot(df, expression[, genes, drop = FALSE], "each", "umap_i_1",
                                             "umap_i_2", 4, "umap_i", dim, dual, legend)
                    built <- plotly_build(p)$x
                    expect_equal_(length(built$data) > 1, position != "hidden",
                                  sprintf("%s, %d genes: colour key unless hidden", label, length(genes)))
                }
            }
        }
    }
})
