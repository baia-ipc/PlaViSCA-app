# Server code of the Plots tab: the number boxes and the plot builder, for the
# cells selected by the tab's filters (inputs with the suffix "_meta").

plots_tab_server <- function(input, output, all_cells) {
  cells <- reactive({
    apply_secondary_filters(all_cells(), collect_filter_inputs(input, suffix = "_meta"))
  })

  output$total_cells_meta <- renderUI({
    value_box(
      title = "Total Cells",
      value = format(nrow(cells()), big.mark = ","),
      showcase = fontawesome::fa("dna"),
      theme = "cells"
    )
  })

  output$num_studies_meta <- renderUI({
    value_box(
      title = "Studies",
      value = length(unique(cells()[[get_data_column("study_label")]])),
      showcase = fontawesome::fa("book"),
      theme = "rna"
    )
  })

  output$num_species_meta <- renderUI({
    value_box(
      title = "Host Species",
      value = length(unique(cells()[[get_data_column("host_species")]])),
      showcase = fontawesome::fa("microscope"),
      theme = "features"
    )
  })

  plot_builder_server(
    "plot_builder", cells,
    show_missing = reactive(!isFALSE(input$show_missing_meta))
  )
}
