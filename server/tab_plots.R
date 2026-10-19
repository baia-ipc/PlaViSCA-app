# Server code of the Metadata plots tab: the number boxes and the plot
# builders, for the cells selected by the tab's filters (`filters`, a reactive
# with the filter values, from shared_filters_server()).
#
# The tab starts with one plot builder ("plot_builder_1"). The "+" button of
# the last builder adds a new one at the end, the "-" button removes its own
# builder; "-" is hidden while only one builder is left.

plots_tab_server <- function(input, output, all_cells, filters) {
  cells <- reactive({
    apply_secondary_filters(all_cells(), filters())
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

  show_missing <- reactive(!isFALSE(input$show_missing_meta))

  # IDs of the builders shown, in order; IDs are never reused
  builder_ids <- reactiveVal("plot_builder_1")
  n_created <- 1

  start_builder <- function(id) {
    handles <- plot_builder_server(id, cells, show_missing)

    observeEvent(handles$add(), {
      n_created <<- n_created + 1
      new_id <- paste0("plot_builder_", n_created)
      insertUI("#metadata_plots_container", where = "beforeEnd",
               ui = metadata_plot_column(new_id))
      start_builder(new_id)
      builder_ids(c(builder_ids(), new_id))
    })

    observeEvent(handles$remove(), {
      removeUI(paste0("#", id, "_column"))
      builder_ids(setdiff(builder_ids(), id))
    })
  }
  start_builder("plot_builder_1")

  # "+" only on the last builder, "-" only when more than one is left
  observeEvent(builder_ids(), {
    ids <- builder_ids()
    for (id in ids) {
      shinyjs::toggle(NS(id, "add"), condition = identical(id, ids[[length(ids)]]))
      shinyjs::toggle(NS(id, "remove"), condition = length(ids) > 1)
    }
  }, ignoreInit = TRUE)
}
