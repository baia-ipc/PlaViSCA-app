# QC Plots tab: RNA / feature counts, PCA elbow plot and variable genes.

ui_tab_qc <- function() {
  tabPanel(
    tagList(fontawesome::fa("vial-circle-check"), " QC Plots"),
    value = "QC Plots",
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
          fetched_plot_output("data_count_plotly", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header("Feature Count distribution"),
          fetched_plot_output("data_feature_plotly", height = "400px")
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        class = "mt-3",
        card(
          full_screen = TRUE,
          card_header("Elbow Plot"),
          fetched_plot_output("data_pca_stdv_plotly", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header("Variable Genes"),
          fetched_plot_output("variable_genes_plotly", height = "400px")
        )
      )
    )
  )
}
