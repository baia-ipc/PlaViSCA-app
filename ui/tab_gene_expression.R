# Gene expression tab: cards with plots of selected genes
# (functions/ui/gene_expression_card.R).

ui_tab_gene_expression <- function() {
  tabPanel(
    tagList(fontawesome::fa("dna"), " Gene expression"),
    value = "Gene expression",
    # Shared filter row
    create_meta_filter_row("_gex"),

    # Plot settings in a sidebar left of the gene panels
    plot_settings_layout(
      tagList(
        data_type_control("data_type_exp"),
        tags$div(
          title = "In the dot plots and the heatmap, groups with fewer cells are left out",
          numericInput(
            "min_cells_exp",
            tags$span("Min. cells per group", class = "settings-label"),
            value = SUMMARY_MIN_CELLS, min = 1, step = 1
          )
        )
      ),
      fluidRow(
        id = "gene_exp_container",
        column(
          width = 6,
          id = "dynamic_col_0",
          gene_expression_card(0)
        )
      )
    )
  )
}
