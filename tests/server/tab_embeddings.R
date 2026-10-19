check("embeddings_tab_server draws the single plot of the filtered cells", {
  testServer(function(input, output, session) {
    tab <- embeddings_tab_server(input, output, session, loaded_data(), reactive(study_filter(STUDY)))
  }, {
    session$setInputs(tabset = "Embeddings", embedding = "umap", check_all = TRUE, plot = "2d",
                      xaxis = umap_i[1], yaxis = umap_i[2], size = 2,
                      color = get_data_column("broad_stage"), check = FALSE)
    stop_unless(nrow(tab$cells()$data) == n_cells(STUDY), "not the cells of the study")
    wait_later(0.7)
    session$flushReact()
    stop_unless(!is.null(output$all_plotly), "no plot")
    stop_unless(isTRUE(tab$plot_drawn()), "plot not marked as drawn")
  })
})

check("embedding_figure colours by a category and by genes", {
  cells <- apply_secondary_filters(norm_df, study_filter(STUDY), keep_gene_matrix = FALSE,
                                   genes = genes(norm_df, gff_data)$gene)
  params <- list(color = get_data_column("broad_stage"), genes = NULL, check = FALSE, combine = NULL,
                 xaxis = umap_i[1], yaxis = umap_i[2], size = 2, plot = "2d")
  stop_unless(inherits(embedding_figure(cells, params, "umap_i", FALSE, NULL, norm_df), "plotly"),
              "no plot by category")
  params$color <- "genes"
  params$genes <- c("CSP", "DBP")
  params$combine <- "mean"
  p <- embedding_figure(cells, params, "umap_i", TRUE, genes(norm_df, gff_data), norm_df)
  stop_unless(inherits(p, "plotly"), "no plot by genes")
  stop_unless(is.null(embedding_figure(cells[0, ], params, "umap_i", TRUE, NULL, norm_df)),
              "a plot without cells")
})

check("the comparison plots are redrawn when their colour changes", {
  testServer(function(input, output, session) {
    tab <- embeddings_tab_server(input, output, session, loaded_data(), reactive(study_filter(STUDY)))
  }, {
    session$setInputs(tabset = "Embeddings", embedding = "umap", check_all = TRUE, plot = "2d",
                      xaxis = umap_i[1], yaxis = umap_i[2], size = 2,
                      color = get_data_column("broad_stage"), check = FALSE)
    session$setInputs(enable_comparison = 1, color_second = get_data_column("broad_stage"))
    stop_unless(grepl(get_data_label("broad_stage"), output$second_plotly, fixed = TRUE), "plot B not by broad stage")
    session$setInputs(color_second = get_data_column("study_label"))
    stop_unless(grepl(get_data_label("study_label"), output$second_plotly, fixed = TRUE), "plot B not redrawn by study")
  })
})
