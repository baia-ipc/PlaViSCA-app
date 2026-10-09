# Data sources tab: the studies in the atlas (data_source.csv).

ui_tab_data_sources <- function() {
  tabPanel(
    tagList(fontawesome::fa("book-open"), " Data sources"),
    value = "Data sources",
    card(
      class = "mt-3",
      card_header(tagList(fontawesome::fa("book"), " Data Sources")),
      div(
        id = "source_section",
        DT::DTOutput("data_source")
      )
    )
  )
}
