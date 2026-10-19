# Comparison mode of the Embeddings tab: the "Compare Plots" button shows two
# plots side by side, A and B, each with its own colour, genes and gene
# combination; they share the embedding, axes and point size. The two plots
# start from the colour (and genes) of the single plot, whose colour is
# restored when the single plot comes back.
# `comparison_mode` (reactiveVal) is TRUE while the two plots are shown;
# `draw_plot(params, dual)` draws a plot (see embeddings_tab_server()).

embedding_compare_server <- function(input, output, session, gene_data, comparison_mode, draw_plot) {
  single_plot_color <- reactiveVal(get_data_column("broad_stage"))
  observe({
    if (!comparison_mode()) single_plot_color(input$color)
  })

  observeEvent(input$enable_comparison, {
    comparing <- !comparison_mode()
    comparison_mode(comparing)
    updateCheckboxInput(session, "comparison_active", value = comparing)
    updateActionButton(session, "enable_comparison",
      label = tagList(fontawesome::fa("columns"), if (comparing) " Single Plot" else " Compare Plots"),
      icon = NULL
    )
    shinyjs::toggleClass("enable_comparison", "btn-primary", condition = comparing)
    shinyjs::toggleClass("enable_comparison", "btn-outline-primary", condition = !comparing)
    shinyjs::toggle("comparison_controls_panel", condition = comparing)
    shinyjs::toggle("comparison_plots_panel", condition = comparing)
    shinyjs::toggle("single_plot_panel", condition = !comparing)

    if (comparing) {
      shinyjs::hide("genes_single_panel")
      shinyjs::runjs("var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')); var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) { return new bootstrap.Tooltip(tooltipTriggerEl); });")

      single_plot_color(input$color)
      updateSelectInput(session, "color_first_comp", selected = input$color)
      updateSelectInput(session, "color_second", selected = input$color)

      # the genes of the single plot, as names or IDs like each menu
      if (identical(input$color, "genes") && length(input$genes_n) > 0) {
        genes_df <- gene_data()$genes
        genes_a <- convert_gene_selection(input$genes_n, genes_df, isTRUE(input$check_first_comp))
        genes_b <- convert_gene_selection(input$genes_n, genes_df, isTRUE(input$check_second))
        later(function() {
          updateSelectizeInput(session, "genes_n_first_comp", selected = genes_a)
          updateSelectizeInput(session, "genes_second", selected = genes_b)
        }, 0.2)
      }
    } else if (!is.null(single_plot_color())) {
      updateSelectInput(session, "color", selected = single_plot_color())
    }
  })

  # settings of plot A and plot B
  plot_params <- function(color_id, genes_id, check_id, combine_id, default_color) {
    reactive({
      req(comparison_mode())
      if (isTRUE(input$plot == "2d")) {
        req(input$xaxis, input$yaxis)
      }
      list(
        color = input[[color_id]] %||% get_data_column(default_color),
        genes = input[[genes_id]] %||% "",
        check = input[[check_id]] %||% FALSE,
        combine = input[[combine_id]],
        xaxis = input$xaxis,
        yaxis = input$yaxis,
        size = input$size %||% 4,
        plot = input$plot %||% "3d"
      )
    })
  }
  params_a <- plot_params("color_first_comp", "genes_n_first_comp", "check_first_comp",
                          "combine_genes_first_comp", "broad_stage")
  params_b <- plot_params("color_second", "genes_second", "check_second",
                          "combine_genes_second", "detailed_stage")

  output$first_plotly <- renderPlotly(draw_plot(params_a(), dual = TRUE))
  output$second_plotly <- renderPlotly(draw_plot(params_b(), dual = TRUE))
}
