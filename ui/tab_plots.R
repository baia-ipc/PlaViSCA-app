# Plots tab: counts of cells by study, stage, host and other metadata.

ui_tab_plots <- function() {
  tabPanel(
    tagList(fontawesome::fa("chart-column"), " Plots"),
    value = "Plots",
    create_meta_filter_row("_meta"),
    layout_column_wrap(
      width = 1 / 3,
      class = "mt-3 mb-3",
      uiOutput("total_cells_meta"),
      uiOutput("num_studies_meta"),
      uiOutput("num_species_meta")
    ),
    div(
      id = "meta_section",
      layout_column_wrap(
        width = 1 / 3,
        card(
          card_header(get_data_label("broad_stage")),
          plotlyOutput("broad_stage_plotly")
        ),
        card(
          card_header(get_data_label("detailed_stage")),
          plotlyOutput("detailed_stage_plotly"),
          card_footer(
            class = "text-muted small italic",
            app_note("stage_prediction")
          )
        ),
        card(card_header(get_data_label("host_species")), plotlyOutput("species_plotly"))
      ),
      layout_column_wrap(
        width = 1 / 3,
        class = "mt-3",
        card(
          card_header(get_data_label("parasite_lineage_isolate")),
          plotlyOutput("lineage_plotly")
        ),
        card(
          card_header(get_data_label("treatment")),
          plotlyOutput("treatment_plotly"),
          card_footer(
            class = "text-muted small italic",
            get_data_help("treatment")
          )
        ),
        card(
          card_header(get_data_label("idc_reference_class")),
          plotlyOutput("idc_reference_class_plotly"),
          card_footer(
            class = "text-muted small italic",
            get_data_help("idc_reference_class")
          )
        )
      ),
      layout_column_wrap(
        width = 1 / 3,
        class = "mt-3",
        card(
          card_header(get_data_label("parasite_origin_location")),
          plotlyOutput("origin_location_plotly"),
          card_footer(
            class = "text-muted small italic",
            get_data_help("parasite_origin_location")
          )
        ),
        card(
          card_header(get_data_label("pred_gametocyte_sex")),
          plotlyOutput("pred_gametocyte_sex_plotly"),
          card_footer(
            class = "text-muted small italic",
            get_data_help("pred_gametocyte_sex")
          )
        ),
        card(
          card_header(get_data_label("source_sex_annotation")),
          plotlyOutput("source_sex_annotation_plotly"),
          card_footer(
            class = "text-muted small italic",
            get_data_help("source_sex_annotation")
          )
        )
      ),
    )
  )
}
