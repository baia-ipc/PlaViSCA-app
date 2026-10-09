# Gene expression tab: violin plots of selected genes by group.

ui_tab_gene_expression <- function() {
  tabPanel(
    tagList(fontawesome::fa("dna"), " Gene expression"),
    value = "Gene expression",
    div(
      class = "gene-expression-section py-2", # Added py-2 for less padding
      style = "overflow: visible;",
      # Removed Header
      # Shared filter row
      create_meta_filter_row("_gex"),

      # Gene expression controls
      create_plot_controls("data_type_exp", FALSE, "_exp"),

      # Note: gene coverage differs between studies
      div(
        class = "alert alert-info py-2 px-3 mt-2 mb-0 small",
        fontawesome::fa("circle-info", height = "12px"),
        tags$span(class = "ms-1", app_note("gene_expression_coverage"))
      ),
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
  )
}
