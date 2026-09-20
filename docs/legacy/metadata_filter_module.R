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

# Metadata Filtering Module
# Functions for handling metadata filters and filtered data displays

#' Initialize metadata filter choices
#' @param mr_data Metadata dataframe
#' @return List of grouped filter choices
initialize_filter_choices <- function(mr_data) {
    list(
        parasite_stages = setNames(
            paste0("parasite_stages__", unique(mr_data$parasite_stages)),
            unique(mr_data$parasite_stages)
        ),
        study_label = setNames(
            paste0("study_label__", unique(mr_data$study_label)),
            unique(mr_data$study_label)
        ),
        development_phase = setNames(
            paste0("development_phase__", unique(mr_data$development_phase)),
            unique(mr_data$development_phase)
        ),
        geographic_location = setNames(
            paste0("geolocation__", unique(mr_data$geographic_location)),
            unique(mr_data$geographic_location)
        ),
        sample_type = setNames(
            paste0("sample_type__", unique(mr_data$sample_type)),
            unique(mr_data$sample_type)
        ),
        sc_technology = setNames(
            paste0("sc_technology__", unique(mr_data$sc_technology)),
            unique(mr_data$sc_technology)
        ),
        sequencer = setNames(
            paste0("sequencer__", unique(mr_data$sequencer)),
            unique(mr_data$sequencer)
        ),
        host_species = setNames(
            paste0("host_species__", unique(mr_data$host_species)),
            format_species_name(unique(mr_data$host_species))
        ),
        life_cycle_stage = setNames(
            paste0("life_cycle_stage__", unique(mr_data$life_cycle_stage)),
            unique(mr_data$life_cycle_stage)
        ),
        treatment = setNames(
            paste0("treatment__", unique(mr_data$treatment)),
            unique(mr_data$treatment)
        )
    )
}

#' Setup metadata filter UI
#' @param session Shiny session object
#' @param grouped_choices List of filter choices
#' @param default_df_filter Reactive value for default filter
setup_metadata_filter_ui <- function(session, grouped_choices, default_df_filter) {
    observe({
        first_group <- names(grouped_choices)[1]
        default_values <- grouped_choices[[first_group]]

        if (is.null(default_df_filter())) {
            default_df_filter(default_values)
        }

        updateSelectInput(
            session,
            "apply_filter",
            choices = grouped_choices,
            selected = character(0)
        )
    })
}

#' Create filtered data reactive
#' @param input Shiny input object
#' @param default_df_filter Reactive value for default filter
#' @param sessionData Session environment for caching
#' @param mr_data Metadata dataframe
#' @return Reactive expression for filtered data
create_filtered_data_reactive <- function(input, default_df_filter, sessionData, mr_data) {
    reactive({
        selections <- input$apply_filter

        # If no selections made (empty or null), use default filter
        if (is.null(selections) || length(selections) == 0) {
            selections <- default_df_filter()
        }

        req(selections)

        # Create cache key for filtered data
        cache_key <- make_cache_key("filtered_metadata", selections)

        # Check cache first to avoid reprocessing
        if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
            return(sessionData[[cache_key]])
        }

        result <- fun_filter_df(mr_data, selections)

        # Cache the result
        sessionData[[cache_key]] <- result

        return(result)
    })
}

#' Update tab-specific filter inputs
#' @param session Shiny session object
#' @param filt_data Filtered data dataframe
#' @param tab_suffix Suffix for input IDs (e.g., "", "_qc", "_exp")
update_filter_inputs <- function(session, filt_data, tab_suffix = "") {
    updateSelectInput(
        session,
        paste0("study_label", tab_suffix),
        choices = unique(filt_data$study_label)
    )
    updateSelectInput(
        session,
        paste0("sample_type", tab_suffix),
        choices = unique(filt_data$sample_type)
    )
    updateSelectizeInput(
        session,
        paste0("host_species", tab_suffix),
        choices = setNames(
            unique(filt_data$host_species),
            format_species_name(unique(filt_data$host_species))
        ),
        server = TRUE
    )
    updateSelectInput(
        session,
        paste0("life_cycle_stage", tab_suffix),
        choices = unique(filt_data$life_cycle_stage)
    )
    updateSelectInput(
        session,
        paste0("treatment", tab_suffix),
        choices = unique(filt_data$treatment)
    )
    updateSelectInput(
        session,
        paste0("parasite_stages", tab_suffix),
        choices = unique(filt_data$parasite_stages)
    )
    updateSelectInput(
        session,
        paste0("development_phase", tab_suffix),
        choices = unique(filt_data$development_phase)
    )
    updateSelectInput(
        session,
        paste0("strain", tab_suffix),
        choices = unique(filt_data$strain)
    )
    updateSelectInput(
        session,
        paste0("hour_post_invasion", tab_suffix),
        choices = sort_numeric_choices(filt_data$hour_post_invasion)
    )
    updateSelectInput(
        session,
        paste0("sequencer", tab_suffix),
        choices = unique(filt_data$sequencer)
    )
    updateSelectInput(
        session,
        paste0("sc_technology", tab_suffix),
        choices = unique(filt_data$sc_technology)
    )
}

#' Render metadata plots and table
#' @param output Shiny output object
#' @param filt_data Filtered data dataframe
render_metadata_outputs <- function(output, filt_data) {
    # Create metadata table
    output$data_table <- renderDT(
        {
            processed_data <- filt_data %>%
                select(
                    -c(
                        starts_with("umap_"),
                        starts_with("pca_"),
                        starts_with("t_sne_"),
                        barcode,
                        # life_cycle_stage,  # Hidden from UI
                        # parasite_stages,
                        geographic_location,
                        day_post_infection
                    )
                ) %>%
                rename_with(
                    ~ str_to_title(.),
                    .cols = any_of(c(
                        "study_label",
                        "sample_type",
                        "host_species",
                        # "life_cycle_stage",
                        "treatment",
                        "strain",
                        "hour_post_invasion",
                        "sequencer",
                        "sc_technology"
                    ))
                ) %>%
                rename(
                    nCount_RNA = n_count_rna,
                    nFeature_RNA = n_feature_rna,
                    Study_Label = Study_label
                ) %>%
                arrange(Study_Label, desc(nCount_RNA)) %>%
                select(
                    Study_Label,
                    Sample_type,
                    Host_species,
                    Strain,
                    Treatment,
                    # Life_cycle_stage,
                    Hour_post_invasion,
                    Sc_technology,
                    Sequencer,
                    nCount_RNA,
                    nFeature_RNA
                )

            DT::datatable(
                processed_data,
                escape = FALSE,
                extensions = "Buttons",
                options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = "frtipB",
                    buttons = list(
                        list(
                            extend = "csv",
                            text = "Export .csv",
                            className = "btn btn-success"
                        )
                    )
                ),
                selection = "single",
                filter = "top",
                rownames = FALSE
            )
        },
        server = TRUE
    )

    # Metadata plots
    output$sample_plotly <- renderPlotly({
        meta_plotly(filt_data, "sample_type", "Count", "Sample type", "bar")
    })

    output$species_plotly <- renderPlotly({
        meta_plotly(filt_data, "host_species", "Count", "Host species", "bar")
    })

    output$strain_plotly <- renderPlotly({
        meta_plotly(filt_data, "strain", "Count", "Strain", "bar")
    })

    output$treatment_plotly <- renderPlotly({
        meta_plotly(filt_data, "treatment", "Count", "Treatment", "bar")
    })

    output$life_cycle_plotly <- renderPlotly({
        meta_plotly(filt_data, "life_cycle_stage", "Count", "Life cyle stage", "bar")
    })

    output$HPI_plotly <- renderPlotly({
        meta_plotly(
            filt_data,
            "hour_post_invasion",
            "Count",
            "Intra erythrocyte development stage",
            "bar"
        )
    })
}
