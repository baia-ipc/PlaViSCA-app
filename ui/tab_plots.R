# Plots tab: the plot builder, which counts the selected cells by one or two
# metadata fields (functions/plot_builder.R). The settings sidebar can hide
# the missing values.

ui_tab_plots <- function() {
  tabPanel(
    tagList(fontawesome::fa("chart-column"), " Plots"),
    value = "Plots",
    create_meta_filter_row("_meta"),
    # Settings of the plots in a sidebar left of them
    plot_settings_layout(
      checkboxInput("show_missing_meta", "Show missing", value = TRUE),
      layout_column_wrap(
        width = 1 / 3,
        class = "mb-3",
        uiOutput("total_cells_meta"),
        uiOutput("num_studies_meta"),
        uiOutput("num_species_meta")
      ),
      plot_builder_ui("plot_builder")
    )
  )
}
