# Analysis section: the tab set holding the analysis tabs (ui/tab_*.R).

ui_analysis_section <- function() {
  div(
    id = "analysis_section",
    style = "display: none;",
    div(id = "particles-js-analysis", class = "particles-js-full"),
    div(
      class = "analysis-dashboard",
      # Main Content for Analysis
      div(
        class = "analysis-content-wrapper",
        div(
          class = "analysis-content w-100",
          style = "overflow: visible; position: relative; z-index: 1;",
          tabsetPanel(
            id = "tabset",

            ui_tab_embeddings(),

            ui_tab_gene_expression(),

            ui_tab_qc(),

            ui_tab_metadata(),

            ui_tab_data_sources()
          )
        )
      )
    )
  )
}
