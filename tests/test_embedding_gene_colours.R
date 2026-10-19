# Tests of the embedding plots coloured by genes
# (functions/plots/embedding_gene_colours.R): combining genes, scaling, mixing one
# colour per gene, and the plots built from them.

needs_gene_colour_functions <- function() {
    if (!exists("blend_gene_colours") || !exists("embedding_plot")) {
        skip_test("embedding functions not loaded in this context")
    }
}

rgb_of <- function(hex) as.numeric(grDevices::col2rgb(hex)) / 255

test_that_("Combine genes: the choices of the Gene expression tab, Each for 2 or 3 genes", {
    needs_gene_colour_functions()
    expect_equal_(names(gene_combine_choices(2)), names(COMBINE_GENES), "same labels as COMBINE_GENES")
    expect_true_("each" %in% gene_combine_choices(3), "Each gene separately for 3 genes")
    expect_true_(!"each" %in% gene_combine_choices(4), "no Each gene separately for 4 genes")
    expect_true_(!"each" %in% gene_combine_choices(1), "no Each gene separately for 1 gene")
    expect_equal_(effective_gene_combine("each", 3), "each", "Each kept for 3 genes")
    expect_equal_(effective_gene_combine("each", 4), "mean", "Each falls back to Mean above 3 genes")
    expect_equal_(effective_gene_combine("sum", 5), "sum", "Sum for any number of genes")
    expect_equal_(effective_gene_combine(NULL, 2), "each", "Each gene separately without a choice, 2 genes")
    expect_equal_(effective_gene_combine(NULL, 5), "mean", "Mean without a choice, 5 genes")
})

test_that_("Combine genes: mean and sum of the genes in each cell", {
    needs_gene_colour_functions()
    expression <- cbind(a = c(1, 0, 2), b = c(3, 0, NA))
    expect_equal_(combine_gene_expression(expression, "mean"), c(2, 0, 2), "mean per cell")
    expect_equal_(combine_gene_expression(expression, "sum"), c(4, 0, 2), "sum per cell")
    expect_equal_(combine_gene_expression(expression[, "a", drop = FALSE], "sum"), c(1, 0, 2),
        "one gene: its expression")
    expect_equal_(gene_colour_title(c("CSP", "AMA1"), "sum"), "Sum of 2 genes", "legend title")
    expect_equal_(gene_colour_title("CSP", "mean"), "CSP", "legend title of one gene")
})

test_that_("expression of the selected genes, by name or ID, for the plotted cells", {
    needs_gene_colour_functions()
    table <- data.frame(PVP01_A = c(1, 2, 3, 4), PVP01_B = c(0, 5, 0, 6), barcode = letters[1:4])
    gene_names <- data.frame(gene = c("PVP01_A", "PVP01_B"), name = c("CSP", "AMA1"))
    by_name <- embedding_gene_expression(c("AMA1", "CSP", "unknown"), FALSE, gene_names, table,
                                         row_idx = c(2, 4))
    expect_equal_(colnames(by_name), c("AMA1", "CSP"), "genes found, named as in the menu")
    expect_equal_(unname(by_name[, "AMA1"]), c(5, 6), "rows of the plotted cells")
    by_id <- embedding_gene_expression("PVP01_B", TRUE, gene_names, table)
    expect_equal_(unname(by_id[, 1]), c(0, 5, 0, 6), "gene ID, all rows")
    expect_true_(is.null(embedding_gene_expression("unknown", FALSE, gene_names, table)),
        "NULL when no gene is found")
})

test_that_("scaling to 0..1 at the 99th percentile", {
    needs_gene_colour_functions()
    x <- c(0:99, 1000)
    s <- scale_gene_expression(x)
    expect_equal_(attr(s, "top"), unname(quantile(x, 0.99)), "full colour at the 99th percentile")
    expect_true_(all(s >= 0 & s <= 1), "values between 0 and 1")
    expect_equal_(s[[101]], 1, "values above the percentile clipped at 1")
    expect_equal_(as.numeric(scale_gene_expression(c(0, 0, NA))), c(0, 0, 0), "no expression: all 0")
    sparse <- scale_gene_expression(c(rep(0, 200), 4))
    expect_equal_(attr(sparse, "top"), 4, "rare expression: scaled by its maximum")
})

test_that_("mixing one colour per gene", {
    needs_gene_colour_functions()
    none <- blend_gene_colours(matrix(0, 1, 2))
    expect_true_(all(abs(rgb_of(none) - GENE_BLEND_NONE) < 0.01), "no expression: light grey")
    red <- rgb_of(blend_gene_colours(matrix(c(1, 0), 1)))
    green <- rgb_of(blend_gene_colours(matrix(c(0, 1), 1)))
    both <- rgb_of(blend_gene_colours(matrix(c(1, 1), 1)))
    expect_true_(red[1] > 0.8 && red[2] < 0.1 && red[3] < 0.1, "gene 1 alone: red")
    expect_true_(green[2] > 0.8 && green[1] < 0.1, "gene 2 alone: green")
    expect_true_(both[1] > 0.7 && both[2] > 0.7 && both[3] < 0.1, "genes 1 and 2: yellow")
    half <- rgb_of(blend_gene_colours(matrix(c(0.5, 0), 1)))
    expect_true_(half[2] > red[2] && half[2] < GENE_BLEND_NONE[2], "half expression: between grey and red")
    all3 <- rgb_of(blend_gene_colours(matrix(1, 1, 3)))
    expect_true_(all(all3 < 0.4), "three genes together: dark grey")
    blue <- rgb_of(blend_gene_colours(matrix(c(0, 0, 1), 1)))
    expect_true_(blue[3] > 0.8 && blue[1] < 0.1, "gene 3 alone: blue")
    failed <- tryCatch(blend_gene_colours(matrix(1, 1, 4)), error = function(e) "error")
    expect_equal_(failed, "error", "at most 3 genes")
})

test_that_("colour keys: grid of 2 genes, legend entries of 3", {
    needs_gene_colour_functions()
    grid <- gene_colour_grid(c("CSP", "AMA1"), c(2, 4), steps = 5)
    expect_equal_(dim(grid$colours), c(5L, 5L), "5 x 5 grid")
    expect_equal_(range(grid$y), c(0, 4), "gene 2 from 0 to its full-colour value")
    expect_equal_(grid$colours[1, 1], blend_gene_colours(matrix(0, 1, 2)), "corner without expression")
    expect_equal_(grid$colours[1, 5], blend_gene_colours(matrix(c(1, 0), 1)), "gene 1 along x")
    legend <- gene_colour_legend(c("CSP", "AMA1", "SBP1"))
    expect_equal_(legend$label, c("CSP", "AMA1", "SBP1", "CSP + AMA1", "CSP + SBP1", "AMA1 + SBP1",
                                  "All three", "None"), "one entry per gene, pair, all and none")
})

test_that_("embedding plots coloured by genes: mean, sum and each gene, 2D and 3D", {
    needs_gene_colour_functions()
    n <- 50
    df <- data.frame(umap_i_1 = rnorm(n), umap_i_2 = rnorm(n), umap_i_3 = rnorm(n),
                     barcode = paste0("cell", seq_len(n)))
    names(df)[names(df) == "barcode"] <- get_data_column("barcode")
    df[[get_data_column("study_label")]] <- "Study"
    df[[get_data_column("detailed_stage")]] <- "Ring"
    expression <- cbind(CSP = runif(n), AMA1 = c(rep(0, n - 1), 5), SBP1 = runif(n))
    for (dim in c("2d", "3d")) {
        for (genes in list(1:2, 1:3)) {
            for (combine in COMBINE_GENES) {
                p <- embedding_gene_plot(df, expression[, genes, drop = FALSE], combine,
                                         "umap_i_1", "umap_i_2", 4, "umap_i", dim)
                built <- plotly_build(p)$x
                cells <- built$data[[1]]
                expect_equal_(length(cells$x), n, sprintf("%s %s %d genes: every cell", combine, dim, length(genes)))
                if (combine == "each") {
                    expect_true_(is.character(cells$marker$color), "one colour per cell")
                    expect_true_(grepl("AMA1 5.00", cells$text[[n]]), "hover shows each gene's value")
                    expect_equal_(as.numeric(cells$key[[n]]), n, "cell keys follow the rows of the data")
                    expect_true_(length(built$data) > 1, "colour key added")
                } else {
                    expect_true_(is.numeric(cells$marker$color), "colour scale of the combined value")
                }
            }
        }
    }
})
