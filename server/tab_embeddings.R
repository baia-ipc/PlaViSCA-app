# Server code of the Embeddings tab: the cells selected by the tab's filters
# (`filters`, a reactive with the filter values, from
# shared_filters_server()), the single embedding plot, the "Download Data"
# button, and (in their own files) the controls (embedding_controls.R), the
# comparison plots (embedding_compare.R) and the cell details
# (cell_details.R). `loaded` is the list returned by data_loading_server().

embeddings_tab_server <- function(input, output, session, loaded, filters) {
  gene_data <- loaded$gene_data
  comparison_mode <- reactiveVal(FALSE)

  # the cells selected by the filters (metadata rows, with attr "row_idx"
  # giving their rows in the expression matrix), and the gene table
  cells <- reactive({
    req(gene_data())
    list(data = filtered_cells(filters()), genes = gene_data()$genes)
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

  # Plot of the cells as set by `params` (see embedding_figure()), or the
  # message shown instead when the filters keep no cell; `dual`
  # for the comparison plots. The plot depends on `params`, the cells and the
  # embedding; the rest is read without dependency. A plot drawn once, by
  # any session, is taken from the cache (cached_embedding_plot()).
  draw_plot <- function(params, dual) {
    force(params)
    plot_data <- cells()$data
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NO_CELLS_MESSAGE)
    }
    type <- plot_type()
    isolate(cached_embedding_plot(
      params, type, dual, filters(), loaded$data_type(),
      function() embedding_figure(plot_data, params, type, dual,
                                  gene_names = gene_data()$genes,
                                  expression = gene_data()$expression)
    ))
  }

  # the rows (in the plotted cells) drawn by each trace of each plot shown,
  # by output id, to find the cell of a point (embedding_hover.R)
  plot_cells <- reactiveValues()
  record_plot <- function(id, p) {
    plot_cells[[id]] <- attr(p, "cell_rows")
    p
  }

  embedding_controls_server(input, output, session, gene_data, comparison_mode)
  embedding_compare_server(input, output, session, gene_data, comparison_mode, draw_plot, record_plot)

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
    if (inherits(plot_data, "try-error") || is.null(plot_data)) {
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
  output$all_plotly_payload <- render_plot_payload("all_plotly", {
    req(!isTRUE(comparison_mode()), plot_inputs_ready(), plot_ready())
    params <- isolate(list(
      color = input$color, genes = input$genes_n, check = input$check, combine = input$combine_genes,
      xaxis = input$xaxis, yaxis = input$yaxis, size = input$size, plot = input$plot,
      legend_pos = input$legend_pos, legend_size = input$legend_size
    ))
    p <- record_plot("all_plotly", draw_plot(params, dual = FALSE))
    plot_drawn(TRUE)
    p
  })

  cell_details_server(input, output, cells, gene_data, plot_drawn, plot_cells)

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
