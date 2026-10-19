# Tests of the Gene expression tab plots: summaries per group
# (functions/data/gene_summaries.R) and every plot type of a card
# (functions/plots/gene_expression.R, gene_distribution.R,
# gene_summaries.R).

needs_gene_plot_functions <- function() {
    if (!exists("gene_group_summary") || !exists("gene_expression_card_plot")) {
        skip_test("plot functions not loaded in this context")
    }
}

col <- function(key) get_data_column(key)

# Two genes of the normalized data, and their expression in all cells.
test_genes <- function(n) head(grep("^PVP01", colnames(n), value = TRUE), 2)

test_that_("summaries per stage and study: cells, detection, means", {
    needs_gene_plot_functions()
    n <- app_data("norm_df")
    genes <- test_genes(n)
    stage <- col("detailed_stage")
    study <- col("study_label")
    expression <- as.matrix(n[, genes, drop = FALSE])
    table <- gene_group_summary(n, expression, stage, study, min_cells = 1)

    known <- !is_missing_value(n[[stage]]) & !is_missing_value(n[[study]])
    expect_true_(all(tapply(table$cells, table$gene, sum) == sum(known)),
        "every cell with a stage and study is counted once per gene")
    first <- table$gene == genes[[1]]
    expect_equal_(sum(table$detected[first]), sum(expression[known, 1] > 0),
        "cells detecting the first gene")
    expect_equal_(sum(table$mean[first] * table$cells[first]), sum(expression[known, 1]),
        "means times cells give the total expression")
    expect_true_(all(table$percent >= 0 & table$percent <= 100), "percentages between 0 and 100")

    table20 <- gene_group_summary(n, expression, stage, study, min_cells = 20)
    expect_true_(all(table20$cells >= 20), "groups with fewer than 20 cells are left out")
    one_field <- gene_group_summary(n, expression, stage, min_cells = 1)
    expect_true_(!"split" %in% names(one_field), "no split column without a split field")
})

test_that_("scaling per gene: mean 0 within each gene, clipped", {
    needs_gene_plot_functions()
    scaled <- scale_per_gene(c(1, 2, 3, 5, 5, 5), factor(c("a", "a", "a", "b", "b", "b")))
    expect_equal_(scaled[1:3], c(-1, 0, 1), "scaled values of a gene")
    expect_equal_(scaled[4:6], c(0, 0, 0), "a gene with one value gets 0")
})

test_that_("every plot type draws, with and without split, one or two genes", {
    needs_gene_plot_functions()
    n <- app_data("norm_df")
    genes <- test_genes(n)
    fg <- list(data = n, full = n, row_idx = seq_len(nrow(n)),
               genes = data.frame(gene = genes, name = genes))
    for (type in GENE_PLOT_TYPES) {
        for (opts in list(
            list(split = NO_SPLIT, combine = "mean", points = NO_SPLIT, value = "percent", scale = TRUE),
            list(split = col("study_label"), combine = "each", points = col("study_label"),
                 value = "mean_detected", scale = FALSE),
            list(split = NO_SPLIT, combine = "sum", points = NO_SPLIT, value = "mean", scale = TRUE)
        )) {
            for (g in list(genes[1], genes)) {
                p <- gene_expression_card_plot(fg, g, TRUE, col("detailed_stage"), type, opts)
                expect_true_(inherits(p, "plotly"), sprintf("%s plot of %d gene(s)", type, length(g)))
                built <- plotly::plotly_build(p)
                expect_true_(length(built$x$data) > 0, sprintf("%s plot has traces", type))
            }
        }
    }
})

test_that_("every gene name of the menus selects one gene", {
    if (!exists("genes") || !exists("get_gene_id")) skip_test("gene functions not loaded in this context")
    g <- genes(app_data("norm_df"), readRDS(app_data_path("gene_annotation.rds")))
    expect_true_(!any(duplicated(g$name)), "gene names are unique")
    choices <- get_gene_choices(g, FALSE)
    expect_equal_(get_gene_id(choices, g), g$gene[match(choices, g$name)], "names map back to their IDs")
})
