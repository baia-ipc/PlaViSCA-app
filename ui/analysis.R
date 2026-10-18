# Analysis section: the tab set holding all tabs (ui/tab_*.R, ui/about.R).

ui_analysis_section <- function() {
  div(
    id = "analysis_section",
    div(id = "particles-js-analysis", class = "particles-js-full"),
    div(
      class = "analysis-dashboard",
      # Main Content for Analysis
      div(
        class = "analysis-content-wrapper",
        div(
          # filters-folded: the Filters cards are folded (www/script.js)
          class = "analysis-content w-100 filters-folded",
          style = "overflow: visible; position: relative; z-index: 1;",
          tabsetPanel(
            id = "tabset",

            ui_tab_embeddings(),
            ui_tab_gene_expression(),
            ui_tab_plots(),
            ui_tab_data_table(),
            ui_tab_qc(),
            ui_tab_data_sources(),
            code_link_tab("Code", "source_code"),
            ui_tab_manual(),
            ui_tab_about()
          )
        )
      )
    )
  )
}
