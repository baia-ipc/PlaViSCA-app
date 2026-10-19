# Server code of the Data sources tab: the table of the studies of the
# atlas (pub_data_table, checked against the data at startup in global.R),
# drawn when the tab is opened.

data_sources_tab_server <- function(input, output) {
  observeEvent(input$tabset, {
    if (input$tabset == "Data sources") {
      output$data_source <- renderDT({
        DT::datatable(
          format_data_sources(pub_data_table),
          escape = FALSE,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = TRUE,
            # Title gets most of the width; Authors are short ("... et al.")
            columnDefs = list(
              list(width = "12%", targets = 2, className = "dt-left text-nowrap"),
              list(width = "45%", targets = 3, className = "dt-left")
            )
          ),
          selection = "single",
          rownames = FALSE
        )
      })
    }
  })
}
