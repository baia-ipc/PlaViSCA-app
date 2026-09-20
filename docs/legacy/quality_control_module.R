# =============================================================================
# HISTORICAL / LEGACY - NOT USED BY THE RUNNING APPLICATION
# =============================================================================
# This file was moved out of functions/ during the Phase-3 app repair
# (2026-09, branch plavisca-app-production-repair). It is retained only as a
# historical record.
#
# It is NOT sourced by global.R and is NOT executed by the app. It predates
# the Phase-2 atlas schema and may reference metadata columns that no longer
# exist (e.g. parasite_stages, development_phase, life_cycle_stage,
# hour_post_invasion, geographic_location).
#
# The ONLY authoritative preprocessing implementation is the separate
# pre_process_data repository. Do not update this file into a second
# pipeline, and do not re-source it from the app.
#
# See audit/phase3_app_repair/REPORT.md for the rationale.
# =============================================================================

# Quality Control Module
# Functions for handling quality control plots and data

#' Render quality control plots
#' @param output Shiny output object
#' @param filt_data Filtered data dataframe
#' @param input Shiny input object
#' @param pca_df PCA data dataframe
#' @param hiv_data Highly variable genes data
#' @param mr_data Full metadata dataframe
render_qc_plots <- function(output, filt_data, input, pca_df, hiv_data, mr_data) {
    # RNA count plot
    output$data_count_plotly <- renderPlotly({
        feature_plot(
            filt_data,
            x = "study_label",
            y = "n_count_rna",
            color = "sample_type",
            y_title = "RNA count",
            title = "RNA Count"
        )
    })

    # Feature count plot
    output$data_feature_plotly <- renderPlotly({
        feature_plot(
            filt_data,
            x = "study_label",
            y = "n_feature_rna",
            color = "sample_type",
            y_title = "nFeature count",
            title = "Feature Count"
        )
    })

    # Elbow plot
    output$data_pca_stdv_plotly <- renderPlotly({
        selected_study <- if (
            !is.null(input$study_label_qc) && length(input$study_label_qc) > 0
        ) {
            input$study_label_qc[1]
        } else {
            unique(mr_data$study_label)[1]
        }

        # Filter PCA data by selected study
        study_pca_df <- pca_df %>%
            filter(study == selected_study)

        elbow_plot(study_pca_df, selected_study)
    })

    # Variable genes plot
    output$variable_genes_plotly <- renderPlotly({
        selected_study <- if (
            !is.null(input$study_label_qc) && length(input$study_label_qc) > 0
        ) {
            input$study_label_qc[1]
        } else {
            unique(mr_data$study_label)[1]
        }

        var_genes_plot(hiv_data, selected_study, top_n = 15)
    })
}

#' Setup quality control observer
#' @param input Shiny input object
#' @param output Shiny output object
#' @param filtered_data Reactive expression for filtered data
#' @param pca_df PCA data dataframe
#' @param hiv_data Highly variable genes data
#' @param mr_data Full metadata dataframe
setup_qc_observer <- function(input, output, filtered_data, pca_df, hiv_data, mr_data) {
    observeEvent(
        list(
            input$data_opt,
            input$study_label_qc,
            input$host_species_qc,
            input$parasite_stages_qc,
            input$treatment_qc,
            input$development_phase_qc,
            input$sample_type_qc,
            input$strain_qc,
            input$hour_post_invasion_qc,
            input$sequencer_qc,
            input$sc_technology_qc
        ),
        {
            current_inputs <- list(
                study_label = input$study_label_qc,
                host_species = input$host_species_qc,
                parasite_stages = input$parasite_stages_qc,
                treatment = input$treatment_qc,
                development_phase = input$development_phase_qc,
                sample_type = input$sample_type_qc,
                strain = input$strain_qc,
                hour_post_invasion = input$hour_post_invasion_qc,
                sequencer = input$sequencer_qc,
                sc_technology = input$sc_technology_qc
            )

            filt_data <- apply_secondary_filters(filtered_data(), current_inputs)

            render_qc_plots(output, filt_data, input, pca_df, hiv_data, mr_data)
        }
    )
}
