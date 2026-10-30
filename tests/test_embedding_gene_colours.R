# Tests of the embedding plots coloured by genes
# (functions/plots/embedding_gene_colours.R, gene_colour_mix.R,
# gene_colour_wheel.R, embedding_gene_keys.R): combining genes, scaling,
# mixing one colour per gene as red, green and blue (2 or 3 genes) or on the
# colour wheel (more), and the plots built from them.

needs_gene_colour_functions <- function() {
    if (!exists("wheel_gene_colours") || !exists("blend_gene_colours") || !exists("embedding_plot")) {
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
    table <- Matrix::Matrix(cbind(PVP01_A = c(1, 2, 3, 4), PVP01_B = c(0, 5, 0, 6)), sparse = TRUE)
    rownames(table) <- letters[1:4]
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
    expect_true_(all(abs(rgb_of(none) - GENE_COLOUR_NONE) < 0.01), "no expression: light grey")
    red <- rgb_of(blend_gene_colours(matrix(c(1, 0), 1)))
    green <- rgb_of(blend_gene_colours(matrix(c(0, 1), 1)))
    both <- rgb_of(blend_gene_colours(matrix(c(1, 1), 1)))
    expect_true_(red[1] > 0.8 && red[2] < 0.1 && red[3] < 0.1, "gene 1 alone: red")
    expect_true_(green[2] > 0.8 && green[1] < 0.1, "gene 2 alone: green")
    expect_true_(both[1] > 0.7 && both[2] > 0.7 && both[3] < 0.1, "genes 1 and 2: yellow")
    half <- rgb_of(blend_gene_colours(matrix(c(0.5, 0), 1)))
    expect_true_(half[2] > red[2] && half[2] < GENE_COLOUR_NONE[2], "half expression: between grey and red")
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
    two <- wheel_gene_colours(rbind(c(1, 0, 0, 0), c(0.3, 0, 0, 0)))
    expect_true_(mean(rgb_of(two$colour[[2]])) > mean(rgb_of(two$colour[[1]])),
                 "lower expression: lighter")
    cells <- wheel_gene_colours(rbind(c(1, 0, 0, 0), c(1, 1, 0, 0), c(1, 1, 1, 0)))
    expect_true_(all(diff(cells$intensity) > 0), "darkness follows the total expression of the cell")
    expect_equal_(cells$intensity[[3]], 1, "the highest total at full darkness")
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
    expect_equal_(dim(key$disc), c(81L, 81L, 4L), "colour disc: RGBA image")
    expect_equal_(key$disc[1, 1, 4], 0, "transparent outside the disc")
    centre <- key$disc[41, 41, 1:3]
    expect_true_(max(centre) - min(centre) <= 2, "grey in the centre")
    top <- grDevices::rgb(key$disc[81, 41, 1], key$disc[81, 41, 2], key$disc[81, 41, 3], maxColorValue = 255)
    expect_true_(hue_distance(hcl_of(top)$h, GENE_WHEEL_HUE_OFFSET) < 5, "gene 1's hue at the top of the disc")
    expect_equal_(nrow(key$spokes), 12L, "one spoke per gene")
})

test_that_("Each gene separately: red, green, blue up to 3 genes, the wheel above", {
    needs_gene_colour_functions()
    scaled <- cbind(c(1, 0.2), c(0, 0.5), c(0, 0))
    mixed <- each_gene_colours(scaled)
    expect_equal_(mixed$colour, blend_gene_colours(scaled), "3 genes: red, green, blue")
    expect_equal_(mixed$intensity, c(1, 0.5), "3 genes: intensity of the strongest gene")
    expect_true_(is.null(mixed$dominant), "3 genes: no dominant gene")
    scaled <- cbind(scaled, c(0, 1))
    expect_equal_(each_gene_colours(scaled)$colour, wheel_gene_colours(scaled)$colour, "4 genes: wheel")
})

test_that_("hover text: expression of each gene and the dominant gene", {
    needs_gene_colour_functions()
    expression <- cbind(CSP = c(3, 0), DBP = c(1, 0))
    wheel <- wheel_gene_colours(cbind(c(0.75, 0), c(0.25, 0)))
    text <- gene_hover_values(expression, wheel$dominant, wheel$share)
    expect_equal_(text[[1]], "CSP 3.00, DBP 1.00<br>Dominant: CSP (75%)", "expressing cell, colour wheel")
    expect_equal_(text[[2]], "CSP 0.00, DBP 0.00", "no dominant gene without expression")
    expect_equal_(gene_hover_values(expression)[[1]], "CSP 3.00, DBP 1.00", "red, green, blue: values only")
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
        for (genes in list(1:2, 1:3, 1:4, 1:8)) {
            for (combine in COMBINE_GENES) {
                p <- embedding_gene_plot(df, expression[, genes, drop = FALSE], combine,
                                         "umap_i_1", "umap_i_2", 4, "umap_i", dim)
                built <- plotly_build(p)$x
                cells <- built$data[[1]]
                expect_equal_(length(cells$x), n, sprintf("%s %s %d genes: every cell", combine, dim, length(genes)))
                if (combine == "each") {
                    expect_true_(is.character(cells$marker$color), "one colour per cell")
                    rows <- attr(p, "cell_rows")[[1]]
                    expect_equal_(sort(rows), seq_len(n), "the plot knows the cell of each point")
                    last <- which(rows == n)
                    expect_equal_(length(last), 1L, "every cell drawn once")
                    expect_true_(grepl("AMA1 5.00", cells$customdata[last]), "hover shows each gene's value")
                    expect_true_(grepl("Expression: %{customdata}", cells$hovertemplate, fixed = TRUE),
                                 "the hover template shows the gene values")
                    expect_true_(is.null(cells$key), "no key per cell")
                    expect_equal_(as.character(cells$text), rep("Ring", n), "the stage of each cell")
                    expect_true_(length(built$data) > 1, "colour key added")
                } else {
                    expect_true_(is.numeric(cells$marker$color), "colour scale of the combined value")
                }
            }
        }
    }
})
