# Gene expression tab: violin plots of selected genes by group.

ui_tab_gene_expression <- function() {
  tabPanel(
    tagList(fontawesome::fa("dna"), " Gene expression"),
    value = "Gene expression",
    # Shared filter row
    create_meta_filter_row("_gex"),

    # Gene expression controls
    create_plot_controls("data_type_exp", FALSE, "_exp"),

    fluidRow(
      id = "gene_exp_container",
      class = "mt-3",
      column(
        width = 6,
        id = "dynamic_col_0",
        subBlock(0)
      )
    )
  )
}
