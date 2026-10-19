# Server code of the QC Plots tab: number boxes, RNA and feature counts of
# the cells selected by the tab's filters (`filters`, a reactive with the
# filter values, from shared_filters_server()), and the elbow and variable
# gene plots of the first study the filters select.

qc_tab_server <- function(output, filters) {
  observeEvent(filters(), {
    filt_data <- apply_secondary_filters(mr_data, filters())

    output$total_cells_qc <- renderUI({
      value_box(
        title = "Total Cells",
        value = format(nrow(filt_data), big.mark = ","),
        showcase = fontawesome::fa("dna"),
        theme = "cells"
      )
    })

    output$mean_rna_qc <- renderUI({
      avg_rna <- if (nrow(filt_data) > 0) round(mean(filt_data$n_count_rna, na.rm = TRUE), 0) else 0
      value_box(
        title = "Mean RNA Counts",
        value = format(avg_rna, big.mark = ","),
        showcase = fontawesome::fa("chart-bar"),
        theme = "rna"
      )
    })

    output$mean_features_qc <- renderUI({
      avg_feat <- if (nrow(filt_data) > 0) round(mean(filt_data$n_feature_rna, na.rm = TRUE), 0) else 0
      value_box(
        title = "Mean Features",
        value = format(avg_feat, big.mark = ","),
        showcase = fontawesome::fa("fingerprint"),
        theme = "features"
      )
    })

    output$data_count_plotly <- renderPlotly({
      feature_plot(
        filt_data,
        x = "study_label",
        y = "n_count_rna",
        color = get_data_column("broad_stage"),
        y_title = "RNA count",
        title = "RNA Count"
      )
    })

    output$data_feature_plotly <- renderPlotly({
      feature_plot(
        filt_data,
        x = "study_label",
        y = "n_feature_rna",
        color = get_data_column("broad_stage"),
        y_title = "nFeature count",
        title = "Feature Count"
      )
    })

    output$data_pca_stdv_plotly <- renderPlotly({
      selected_study <- qc_study(filters())
      elbow_plot(pca_df %>% filter(study == selected_study), selected_study)
    })

    output$variable_genes_plotly <- renderPlotly({
      var_genes_plot(hiv_data, qc_study(filters()), top_n = 15)
    })
  })
}

# Study of the elbow and variable gene plots: the first study kept by the
# filter values `filter_values`, or the first study of the atlas.
qc_study <- function(filter_values) {
  col <- get_data_column("study_label")
  studies <- unique(as.character(mr_data[[col]]))
  kept <- filter_effective_values(filter_values[[col]], col %in% attr(filter_values, "exclude"), studies)
  if (length(kept) > 0) kept[1] else studies[1]
}
