# Gene expression tab: violin plots of selected genes by group.

ui_tab_gene_expression <- function() {
  tabPanel(
    tagList(fontawesome::fa("dna"), " Gene expression"),
    value = "Gene expression",
    # Shared filter row
    create_meta_filter_row("_gex"),

    # Plot settings in a sidebar left of the gene panels
    plot_settings_layout(
      data_type_control("data_type_exp"),
      fluidRow(
        id = "gene_exp_container",
        column(
          width = 6,
          id = "dynamic_col_0",
          subBlock(0)
        )
      )
    )
  )
}
