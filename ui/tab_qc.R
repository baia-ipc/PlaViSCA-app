# Quality control tab: RNA / feature counts, PCA elbow plot and variable genes.

ui_tab_qc <- function() {
  tabPanel(
    tagList(fontawesome::fa("vial-circle-check"), " Quality control"),
    value = "Quality control",
    create_meta_filter_row("_qc"),
    layout_column_wrap(
      width = 1 / 3,
      class = "mt-3 mb-3",
      uiOutput("total_cells_qc"),
      uiOutput("mean_rna_qc"),
      uiOutput("mean_features_qc")
    ),
    div(
      id = "qual_section",
      layout_column_wrap(
        width = 1 / 2,
        card(
          full_screen = TRUE,
          card_header("RNA Count distribution"),
          withSpinner(
            type = 6,
            size = 0.5,
            color = "#3498db",
            plotlyOutput("data_count_plotly")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Feature Count distribution"),
          withSpinner(
            type = 6,
            size = 0.5,
            color = "#3498db",
            plotlyOutput("data_feature_plotly")
          )
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        class = "mt-3",
        card(
          full_screen = TRUE,
          card_header("Elbow Plot"),
          plotlyOutput("data_pca_stdv_plotly")
        ),
        card(
          full_screen = TRUE,
          card_header("Variable Genes"),
          plotlyOutput("variable_genes_plotly")
        )
      )
    )
  )
}
