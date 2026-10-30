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
    stop_unless(!is.null(output$all_plotly_payload$json), "no plot")
    stop_unless(isTRUE(tab$plot_drawn()), "plot not marked as drawn")
  })
})

check("embedding_figure colours by a category and by genes", {
  expression <- NORMALIZED$expression
  cells <- apply_secondary_filters(mr_data, study_filter(STUDY))
  params <- list(color = get_data_column("broad_stage"), genes = NULL, check = FALSE, combine = NULL,
                 xaxis = umap_i[1], yaxis = umap_i[2], size = 2, plot = "2d")
  stop_unless(inherits(embedding_figure(cells, params, "umap_i", FALSE, NULL, expression), "plotly"),
              "no plot by category")
  params$color <- "genes"
  params$genes <- c("CSP", "DBP")
  params$combine <- "mean"
  p <- embedding_figure(cells, params, "umap_i", TRUE, NORMALIZED$genes, expression)
  stop_unless(inherits(p, "plotly"), "no plot by genes")
  stop_unless(is.null(embedding_figure(cells[0, ], params, "umap_i", TRUE, NULL, expression)),
              "a plot without cells")
})

check("filtered_cells and cached_embedding_plot are computed once for the same settings", {
  filters <- study_filter(STUDY)
  first <- filtered_cells(filters)
  stop_unless(nrow(first) == n_cells(STUDY), "not the cells of the study")
  stop_unless(identical(filtered_cells(study_filter(STUDY)), first), "second request differs")
  stop_unless(nrow(filtered_cells(empty_filter_inputs())) == nrow(mr_data), "empty filters drop cells")

  params <- list(color = get_data_column("broad_stage"), genes = NULL, check = FALSE, combine = "each",
                 xaxis = umap_i[1], yaxis = umap_i[2], size = "2", plot = "3d",
                 legend_pos = "auto", legend_size = "M")
  built <- 0
  build <- function() {
    built <<- built + 1
    plotly::plotly_empty()
  }
  cached_embedding_plot(params, "umap_i", FALSE, filters, "normalize", build)
  params$xaxis <- umap_i[2]
  cached_embedding_plot(params, "umap_i", FALSE, filters, "normalize", build)
  stop_unless(built == 1, "a 3D plot rebuilt for the same settings")
  params$plot <- "2d"
  cached_embedding_plot(params, "umap_i", FALSE, filters, "normalize", build)
  cached_embedding_plot(params, "umap_i", TRUE, filters, "normalize", build)
  cached_embedding_plot(params, "umap_i", TRUE, filters, "raw", build)
  stop_unless(built == 4, "plots of other settings not built")
})

check("the plot drawn at startup is the one the Embeddings tab opens with", {
  warm_embedding_plot_cache()
  defaults <- EMBEDDING_PLOT_DEFAULTS
  params <- defaults[c("color", "genes", "check", "combine", "xaxis", "yaxis", "size", "plot",
                       "legend_pos", "legend_size")]
  p <- cached_embedding_plot(params, "umap_i", FALSE, empty_filter_inputs(), defaults$data_type,
                             function() stop("the default plot is not in the cache"))
  stop_unless(inherits(p, "plotly"), "no default plot")
})

check("the comparison plots are redrawn when their colour changes", {
  testServer(function(input, output, session) {
    tab <- embeddings_tab_server(input, output, session, loaded_data(), reactive(study_filter(STUDY)))
  }, {
    session$setInputs(tabset = "Embeddings", embedding = "umap", check_all = TRUE, plot = "2d",
                      xaxis = umap_i[1], yaxis = umap_i[2], size = 2,
                      color = get_data_column("broad_stage"), check = FALSE)
    session$setInputs(enable_comparison = 1, color_second = get_data_column("broad_stage"))
    stop_unless(grepl(get_data_label("broad_stage"), output$second_plotly_payload$json, fixed = TRUE), "plot B not by broad stage")
    session$setInputs(color_second = get_data_column("study_label"))
    stop_unless(grepl(get_data_label("study_label"), output$second_plotly_payload$json, fixed = TRUE), "plot B not redrawn by study")
  })
})

check("the single plot is redrawn when the legend position changes", {
  testServer(function(input, output, session) {
    tab <- embeddings_tab_server(input, output, session, loaded_data(), reactive(study_filter(STUDY)))
  }, {
    session$setInputs(tabset = "Embeddings", embedding = "umap", check_all = TRUE, plot = "2d",
                      xaxis = umap_i[1], yaxis = umap_i[2], size = 2,
                      color = get_data_column("broad_stage"), check = FALSE)
    wait_later(0.7)
    session$flushReact()
    stop_unless(grepl("\"orientation\":\"v\"", output$all_plotly_payload$json, fixed = TRUE), "legend not over the plot")
    session$setInputs(legend_pos = "below", legend_size = "L")
    wait_later(0.7)
    session$flushReact()
    stop_unless(grepl("\"orientation\":\"h\"", output$all_plotly_payload$json, fixed = TRUE), "legend not moved below the plot")
  })
})
