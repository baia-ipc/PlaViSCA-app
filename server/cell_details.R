# Cell details of the Embeddings tab: a double-click or right-click on a cell
# of an embedding plot (www/script.js sends embedding_cell_open: the output
# id of the plot, the trace and the point) opens a window with the genes the
# cell expresses, and an "Export .csv" button. `cells` gives the plotted
# cells (reactive, list with $data, from embeddings_tab_server()),
# `gene_data` the loaded data (from data_loading_server()); `plot_drawn()`
# is TRUE once the single plot has been drawn; `plot_cells` (reactiveValues)
# holds, by output id, the rows of the plotted cells drawn by each trace of
# the plot shown (embedding_hover.R).

cell_details_server <- function(input, output, cells, gene_data, plot_drawn, plot_cells) {
  selected_table <- reactiveVal(NULL)

  observeEvent(input$embedding_cell_open, {
    req(plot_drawn())
    opened <- input$embedding_cell_open
    req(opened$plot)
    position <- plotted_cell(plot_cells[[opened$plot]], opened$curve, opened$point)
    req(!is.na(position))

    expression <- gene_data()$expression
    row <- cell_row(position, cells()$data, expression)
    selected_table(cell_expression_table(row, expression, gff_data))

    found <- !is.na(row)
    barcode <- if (found) rownames(expression)[row] else "not found"
    close_button <- modalButton(label = tagList(icon("times")))
    showModal(modalDialog(
      title = paste("Selected cell ID:", barcode),
      div(class = "width-100", DT::DTOutput("cell_expression_table")),
      size = "xl",
      easyClose = TRUE,
      footer = if (found) {
        tagList(downloadButton("export_data", "Export .csv", class = "btn-success"), close_button)
      } else {
        close_button
      }
    ))
  })

  output$cell_expression_table <- DT::renderDT({
    req(selected_table())
    datatable(
      selected_table(),
      options = list(
        pageLength = 15,
        scrollY = "400px",
        scrollCollapse = TRUE,
        paging = TRUE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      filter = "top",
      selection = "single",
      width = "100%"
    ) %>%
      formatRound("exp_value", digits = 3)
  })

  output$export_data <- downloadHandler(
    filename = function() paste("cell_expression_data", Sys.Date(), ".csv"),
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.4, detail = "Saving file...")
        write.csv(selected_table(), file, row.names = FALSE)
        incProgress(1, detail = "Download complete")
      })
    }
  )

  selected_table
}
