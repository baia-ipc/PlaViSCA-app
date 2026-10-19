# Metadata table tab: one row per cell with its metadata, exportable as CSV.

ui_tab_data_table <- function() {
  tabPanel(
    tagList(fontawesome::fa("table-list"), " Metadata table"),
    value = "Metadata table",
    create_meta_filter_row("_table"),
    card(
      class = "mt-2",
      withSpinner(
        type = 6,
        size = 0.5,
        color = "#3498db",
        DT::DTOutput("data_table")
      )
    )
  )
}
