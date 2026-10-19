# Server code of the Embeddings tab: the cells selected by the tab's filters
# (`filters`, a reactive with the filter values, from
# shared_filters_server()), the single embedding plot, the "Download Data"
# button, and (in their own files) the controls (embedding_controls.R), the
# comparison plots (embedding_compare.R) and the cell details
# (cell_details.R). `loaded` is the list returned by data_loading_server().

embeddings_tab_server <- function(input, output, session, loaded, filters) {
  gene_data <- loaded$gene_data
  comparison_mode <- reactiveVal(FALSE)

  # the cells selected by the filters, kept per data type and selection
  cache <- new.env(parent = emptyenv())
  cells <- reactive({
    req(gene_data())
    current_filters <- filters()
    cache_key <- make_cache_key("cells", loaded$data_type(), current_filters,
                                list(exclude = attr(current_filters, "exclude")))
    if (exists(cache_key, envir = cache, inherits = FALSE)) {
      return(cache[[cache_key]])
    }

    # no fallback to unfiltered data: if filtering fails, data and
    # configuration do not match, and showing all cells would mislead
    meta <- apply_secondary_filters(
      gene_data()$data,
      current_filters,
      keep_gene_matrix = FALSE,
      genes = gene_data()$genes$gene
    )
    cache[[cache_key]] <- list(data = meta, genes = gene_data()$genes)
    cache[[cache_key]]
  })

  plot_type <- reactive({
    integrated <- isTRUE(input$check_all)
    switch(input$embedding %||% "umap",
      "pca" = if (integrated) "pca_i" else "pca_u",
      "umap" = if (integrated) "umap_i" else "umap_u",
      "tsne" = "tSNE",
      "umap_i"
    )
  })

  # Plot of the cells as set by `params` (see embedding_figure()); `dual`
  # for the comparison plots. The plot depends on `params`, the cells and the
  # embedding; the rest is read without dependency.
  draw_plot <- function(params, dual) {
    force(params)
    plot_data <- cells()$data
    type <- plot_type()
    isolate(embedding_figure(
      plot_data, params, type, dual,
      gene_names = gene_data()$genes,
      expression_table = loaded$cached_data[[loaded$data_type()]] %||% gene_data()$data
    ))
  }

  embedding_controls_server(input, output, session, gene_data, comparison_mode)
  embedding_compare_server(input, output, session, gene_data, comparison_mode, draw_plot)

  # The single plot is drawn once its inputs have not changed for 0.6 s and,
  # when coloured by genes, the gene selection has stayed the same for 0.15 s.
  inputs_settled <- reactiveVal(TRUE)
  observe({
    list(
      input$embedding, input$plot, input$xaxis, input$yaxis, input$color, input$size,
      input$legend_pos, input$legend_size,
      input$genes_n, input$combine_genes, input$data_type, input$check, input$check_all,
      lapply(filter_field_keys(), function(k) input[[k]])
    )
    inputs_settled(FALSE)
    later(function() inputs_settled(TRUE), 0.6)
  })

  last_gene_selection <- reactiveVal("")
  plot_inputs_ready <- reactive({
    plot_data <- try(cells()$data, silent = TRUE)
    if (inherits(plot_data, "try-error") || is.null(plot_data) || nrow(plot_data) == 0) {
      return(FALSE)
    }
    if (is.null(input$xaxis) || is.null(input$yaxis) || !inputs_settled()) {
      return(FALSE)
    }
    if (input$color == "genes") {
      genes <- input$genes_n
      if (length(genes) == 0 || any(genes == "")) {
        return(FALSE)
      }
      if (!identical(last_gene_selection(), genes)) {
        last_gene_selection(genes)
        invalidateLater(150)
        return(FALSE)
      }
    }
    TRUE
  })
  plot_ready <- reactiveVal(FALSE)
  observe({
    ready <- try(plot_inputs_ready(), silent = TRUE)
    if (!inherits(ready, "try-error")) plot_ready(ready)
  })

  plot_drawn <- reactiveVal(FALSE)
  output$all_plotly <- renderPlotly({
    req(!isTRUE(comparison_mode()), plot_inputs_ready(), plot_ready())
    params <- isolate(list(
      color = input$color, genes = input$genes_n, check = input$check, combine = input$combine_genes,
      xaxis = input$xaxis, yaxis = input$yaxis, size = input$size, plot = input$plot,
      legend_pos = input$legend_pos, legend_size = input$legend_size
    ))
    p <- draw_plot(params, dual = FALSE)
    plot_drawn(TRUE)
    p
  })

  cell_details_server(input, output, cells, gene_data, plot_drawn)

  # "Download Data": the metadata of the cells of the atlas
  output$download_data <- downloadHandler(
    filename = function() {
      base_name <- "Plasmodium_vivax_embedding_data"
      genes <- input$genes_n
      if (identical(input$color, "genes") && length(genes) > 0) {
        base_name <- paste0(base_name, "_", if (length(genes) == 1) genes[1] else paste0(length(genes), "_genes"))
      }
      paste0(base_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.4, detail = "Saving file...")
        write.csv(mr_data, file, row.names = FALSE)
        incProgress(1, detail = "Download complete")
      })
    }
  )

  invisible(list(cells = cells, comparison_mode = comparison_mode, plot_drawn = plot_drawn))
}
