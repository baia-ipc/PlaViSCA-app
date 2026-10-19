# Tests of the embedding plots coloured by genes
# (functions/plots/embedding_gene_colours.R, gene_colour_wheel.R): combining
# genes, scaling, mixing one colour per gene on the colour wheel, and the
# plots built from them.

needs_gene_colour_functions <- function() {
    if (!exists("wheel_gene_colours") || !exists("embedding_plot")) {
        skip_test("embedding functions not loaded in this context")
    }
}

rgb_of <- function(hex) as.numeric(grDevices::col2rgb(hex)) / 255

# Hue (degrees) and chroma of hex colours in HCL, the polar form of CIELUV.
hcl_of <- function(hex) {
    luv <- grDevices::convertColor(t(grDevices::col2rgb(hex)) / 255, from = "sRGB", to = "Luv")
    list(h = (atan2(luv[, 3], luv[, 2]) * 180 / pi) %% 360, c = sqrt(luv[, 2]^2 + luv[, 3]^2))
}

# Smallest difference between two hues, in degrees.
hue_distance <- function(a, b) abs((a - b + 180) %% 360 - 180)

test_that_("Combine genes: the choices of the Gene expression tab, Each for 2 to 8 genes", {
    needs_gene_colour_functions()
    expect_equal_(names(gene_combine_choices(2)), names(COMBINE_GENES), "same labels as COMBINE_GENES")
    expect_true_("each" %in% gene_combine_choices(8), "Each gene separately for 8 genes")
    expect_true_(!"each" %in% gene_combine_choices(9), "no Each gene separately for 9 genes")
    expect_true_(!"each" %in% gene_combine_choices(1), "no Each gene separately for 1 gene")
    expect_equal_(effective_gene_combine("each", 5), "each", "Each kept for 5 genes")
    expect_equal_(effective_gene_combine("each", 9), "mean", "Each falls back to Mean above 8 genes")
    expect_equal_(effective_gene_combine("sum", 9), "sum", "Sum for any number of genes")
    expect_equal_(effective_gene_combine(NULL, 2), "each", "Each gene separately without a choice, 2 genes")
    expect_equal_(effective_gene_combine(NULL, 9), "mean", "Mean without a choice, 9 genes")
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

test_that_("colour wheel: hues of the genes and the colour of a cell", {
    needs_gene_colour_functions()
    for (k in 2:8) {
        hues <- wheel_gene_hues(k)
        expect_equal_(hues[[1]], GENE_WHEEL_HUE_OFFSET, sprintf("%d genes: gene 1 at the offset", k))
        expect_equal_(diff(hues[1:2]), 360 / k, sprintf("%d genes: evenly spaced", k))
    }
    alone <- wheel_gene_colours(diag(5))
    found <- hcl_of(alone$colour)
    expect_true_(all(hue_distance(found$h, wheel_gene_hues(5)) < 3), "one gene alone: its hue")
    expect_true_(all(found$c > 40), "one gene alone: strong colour")
    expect_equal_(alone$dominant, 1:5, "one gene alone: dominant")
    expect_equal_(alone$share, rep(1, 5), "one gene alone: all of the expression")
    pair <- wheel_gene_colours(matrix(c(1, 1, 0, 0, 0, 0), 1))
    expect_true_(hue_distance(hcl_of(pair$colour)$h, mean(wheel_gene_hues(6)[1:2])) < 3,
                 "two neighbouring genes: the hue halfway between them")
    expect_equal_(pair$share, 0.5, "two genes equal: half each")
    for (k in 2:8) {
        expect_true_(hcl_of(wheel_gene_colours(matrix(1, 1, k))$colour)$c < 2,
                     sprintf("%d genes equal: grey", k))
    }
    weak <- wheel_gene_colours(matrix(c(0.3, 0), 1))
    expect_true_(mean(rgb_of(weak$colour)) > mean(rgb_of(alone$colour[[1]])),
                 "lower expression: lighter")
})

test_that_("colour wheel: 3 genes red, green, blue", {
    needs_gene_colour_functions()
    rgb <- vapply(wheel_gene_colours(diag(3))$colour, rgb_of, numeric(3))
    expect_true_(which.max(rgb[, 1]) == 1 && rgb[1, 1] > 0.7, "gene 1: red")
    expect_true_(which.max(rgb[, 2]) == 2 && rgb[2, 2] > 0.5, "gene 2: green")
    expect_true_(which.max(rgb[, 3]) == 3 && rgb[3, 3] > 0.8, "gene 3: blue")
})

test_that_("colour wheel: no expression, rare genes and many cells", {
    needs_gene_colour_functions()
    none <- wheel_gene_colours(matrix(0, 2, 4))
    expect_true_(all(abs(rgb_of(none$colour[[1]]) - GENE_COLOUR_NONE) < 0.01), "no expression: light grey")
    expect_true_(all(is.na(none$dominant)) && all(is.na(none$share)), "no expression: no dominant gene")
    n <- 2000
    expression <- cbind(rexp(n), 0, c(rep(0, n - 1), 3), runif(n))
    scaled <- vapply(1:4, function(g) as.numeric(scale_gene_expression(expression[, g])), numeric(n))
    expect_true_(all(is.finite(scaled)), "a gene with no or rare expression scales without NaN")
    wheel <- wheel_gene_colours(scaled)
    expect_equal_(length(wheel$colour), n, "one colour per cell")
    expect_true_(all(grepl("^#[0-9A-F]{6}$", wheel$colour)), "valid hex colours")
    expect_true_(all(wheel$intensity >= 0 & wheel$intensity <= 1), "intensity between 0 and 1")
    failed <- tryCatch(wheel_gene_colours(matrix(0, 3, 0)), error = function(e) "error")
    expect_equal_(failed, "error", "at least 1 gene")
})

test_that_("colour wheel key: genes at their hues, gene 1 at the top", {
    needs_gene_colour_functions()
    key <- gene_wheel_key(c("CSP", "DBP", "P47", "AMA1"))
    expect_equal_(key$genes$label, c("CSP", "DBP", "P47", "AMA1"), "one point per gene")
    expect_equal_(round(c(key$genes$x[[1]], key$genes$y[[1]]), 6), c(0, 1), "gene 1 at the top")
    expect_equal_(round(key$genes$x[[2]], 6), 1, "gene 2 on the right: clockwise")
    expect_equal_(key$genes$position, c("top center", "middle right", "bottom center", "middle left"),
                  "names outside the wheel")
    expect_true_(all(abs(sqrt(key$ring$x^2 + key$ring$y^2) - 1) < 1e-9), "ring of radius 1")
})

test_that_("hover text: expression of each gene and the dominant gene", {
    needs_gene_colour_functions()
    expression <- cbind(CSP = c(3, 0), DBP = c(1, 0))
    wheel <- wheel_gene_colours(cbind(c(0.75, 0), c(0.25, 0)))
    text <- wheel_hover_values(expression, wheel)
    expect_equal_(text[[1]], "CSP 3.00, DBP 1.00<br>Dominant: CSP (75%)", "expressing cell")
    expect_equal_(text[[2]], "CSP 0.00, DBP 0.00", "no dominant gene without expression")
})

test_that_("embedding plots coloured by genes: mean, sum and each gene, 2D and 3D", {
    needs_gene_colour_functions()
    n <- 50
    df <- data.frame(umap_i_1 = rnorm(n), umap_i_2 = rnorm(n), umap_i_3 = rnorm(n),
                     barcode = paste0("cell", seq_len(n)))
    names(df)[names(df) == "barcode"] <- get_data_column("barcode")
    df[[get_data_column("study_label")]] <- "Study"
    df[[get_data_column("detailed_stage")]] <- "Ring"
    expression <- cbind(CSP = runif(n), AMA1 = c(rep(0, n - 1), 5), SBP1 = runif(n),
                        DBP = runif(n), P47 = 0, MSP1 = runif(n), TRAP = runif(n), CelTOS = runif(n))
    for (dim in c("2d", "3d")) {
        for (genes in list(1:2, 1:3, 1:8)) {
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
