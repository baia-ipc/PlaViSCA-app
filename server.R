library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)
suppressPackageStartupMessages(library(conflicted))
suppressPackageStartupMessages(library(tidyverse))
library(later)
library(writexl)

# Resolve namespace conflicts - set these AFTER loading packages
suppressMessages({
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  conflict_prefer("show", "methods")
  conflict_prefer("layout", "plotly")
})

# Helper function to create empty plots without warnings
create_empty_plot <- function(title = "") {
  plotly::plot_ly() %>%
    plotly::add_trace(
      x = c(),
      y = c(),
      type = "scatter",
      mode = "markers"
    ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      showlegend = FALSE
    )
}

server <- function(input, output, session) {
  sessionData <- new.env(parent = emptyenv())

  # Reactive value for comparison mode (toggled by action button)
  comparison_mode <- reactiveVal(FALSE)

  # Reactive values to preserve UI state across mode changes
  ui_state <- reactiveValues(
    stored_color = "development_phase",
    stored_genes = NULL,
    stored_check = FALSE
  )

  # Load publication data
  pub_data <- reactive({
    readr::read_csv("data/data_source.csv", show_col_types = FALSE)
  })

  observeEvent(filtered_gene_exp_data(), {
    try(
      {
        df_gene0(NULL)
      },
      silent = TRUE
    )
  })
  # Navigate home and data section
  observeEvent(input$home, {
    shinyjs::hide("analysis_section")
    shinyjs::hide("about_section")
    shinyjs::show("home_section")

    shinyjs::runjs(
      "
    $('.menu-link').removeClass('active');
    $('#home').addClass('active');

    // Initialize particles for home page footer
    setTimeout(function() {
      if (typeof initHomeParticles === 'function') {
        initHomeParticles();
      } else if (typeof initParticles === 'function') {
        initParticles('particles-js-home');
      }
    }, 300);
  "
    )
  })

  observeEvent(
    list(input$about, input$contribute_link, input$about_link),
    {
      shinyjs::hide("home_section")
      shinyjs::hide("analysis_section")
      shinyjs::show("about_section")

      runjs("initParticles('colab-particles');")
      runjs("initParticles('particles-js-about');")

      shinyjs::runjs(
        "
      $('.menu-link').removeClass('active');
      $('#about').addClass('active');
      "
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(input$analysis, {
    shinyjs::hide("home_section")
    shinyjs::hide("about_section")
    shinyjs::show("analysis_section")

    shinyjs::runjs(
      "
    $('.menu-link').removeClass('active');
    $('#analysis').addClass('active');
  "
    )
  })

  # Navigation between main sections
  observeEvent(input$metadata_link, {
    navigate_tabset(session, "analysis", "Metadata")
  })

  observeEvent(input$analysis_link, {
    navigate_tabset(session, "analysis", "Embeddings")
  })

  observeEvent(input$qc_link, {
    navigate_tabset(session, "analysis", "Quality control")
  })

  observeEvent(input$sources_link, {
    navigate_tabset(session, "analysis", "Data sources")
  })

  observeEvent(input$gene_exp_link, {
    navigate_tabset(session, "analysis", "Gene expression")
  })

  addResourcePath("manual", "manual/docs/build/html")
  observeEvent(input$usermanual_link, {
    runjs("window.open('manual/index.html', '_blank')")
  })

  # Synchronize plot controls (data type, size, dimension) across tabs
  observeEvent(input$tabset, {
    syncing_tabs(TRUE)

    # Define mapping between tab names and their input suffixes
    tab_suffixes <- list(
      "Embeddings" = "",
      "Gene expression" = "_exp",
      "Quality control" = "_qc",
      "Metadata" = "_meta"
    )

    current_tab <- input$tabset

    # Ensure gene is selected when navigating to Gene expression tab
    if (current_tab == "Gene expression") {
      if (is.null(input$genes_exp_0) || length(input$genes_exp_0) == 0) {
        req(gene_data()$genes)
        choices <- get_gene_choices(gene_data()$genes, input$check_exp_0)
        if (length(choices) > 0) {
          updateSelectizeInput(session, "genes_exp_0", selected = choices[1])
        }
      }
    }

    if (current_tab %in% names(tab_suffixes)) {
      source_suffix <- tab_suffixes[[current_tab]]

      # Determine current global values from the active tab's inputs
      # If they are NULL (e.g., initial load), use defaults
      current_data_type <- if (!is.null(input[[paste0("data_type", source_suffix)]])) {
        input[[paste0("data_type", source_suffix)]]
      } else {
        "normalize"
      }
      current_size <- if (!is.null(input[[paste0("size", source_suffix)]])) {
        input[[paste0("size", source_suffix)]]
      } else {
        4
      }
      current_plot <- if (!is.null(input[[paste0("plot", source_suffix)]])) {
        input[[paste0("plot", source_suffix)]]
      } else {
        "3d"
      }

      # Update all other tabs
      for (tab in names(tab_suffixes)) {
        if (tab != current_tab) {
          target_suffix <- tab_suffixes[[tab]]

          # Sync data type
          if (is.null(input[[paste0("data_type", target_suffix)]]) ||
            input[[paste0("data_type", target_suffix)]] != current_data_type) {
            updateRadioButtons(session, paste0("data_type", target_suffix), selected = current_data_type)
          }

          # Sync size
          if (is.null(input[[paste0("size", target_suffix)]]) ||
            input[[paste0("size", target_suffix)]] != current_size) {
            updateRadioButtons(session, paste0("size", target_suffix), selected = current_size)
          }

          # Sync plot dimension
          if (is.null(input[[paste0("plot", target_suffix)]]) ||
            input[[paste0("plot", target_suffix)]] != current_plot) {
            updateRadioButtons(session, paste0("plot", target_suffix), selected = current_plot)
          }
        }
      }
    }

    # Reset flag after a short delay
    later(
      function() {
        syncing_tabs(FALSE)
      },
      0.1
    )
  })

  # Data caching system for performance optimization
  default_df_filter <- reactiveVal(NULL)
  app_initialized <- reactiveVal(FALSE)
  data_loading_trigger <- reactiveVal(0)
  syncing_tabs <- reactiveVal(FALSE)
  global_loading <- reactiveVal(FALSE)

  cached_data <- reactiveValues(
    raw = NULL,
    normalize = NULL,
    current_type = NULL,
    current_filter = NULL
  )

  # get current data type regardless of tab
  get_current_data_type <- reactive({
    if (input$tabset == "Gene expression") {
      input$data_type_exp
    } else if (input$tabset == "Embeddings") {
      input$data_type
    } else {
      "normalize" # default when launching
    }
  })

  # Load data with performance optimization and caching
  load_dataset <- function(data_type) {
    if (!is.null(cached_data[[data_type]])) {
      return(cached_data[[data_type]])
    }

    dataset <- switch(data_type,
      raw = {
        if (exists("DATA_PATHS") && !is.null(DATA_PATHS$raw_df)) {
          load_data(DATA_PATHS$raw_df)
        } else {
          NULL
        }
      },
      normalize = {
        if (exists("norm_df")) norm_df else NULL
      }
    )

    if (!is.null(dataset)) {
      cached_data[[data_type]] <- dataset
      # Only gc after loading large datasets
      if (data_type == "raw") {
        gc(verbose = FALSE)
      }
    }

    return(dataset)
  }

  # Debounced observer to prevent multiple rapid triggers
  observe({
    list(
      input$data_type,
      input$data_type_exp
    )

    # Only trigger after app is properly initialized
    if (!is.null(input$data_type) || !is.null(input$data_type_exp)) {
      if (!app_initialized()) {
        app_initialized(TRUE)
      }
    }

    # Use later() to debounce - only trigger after 300ms
    if (app_initialized()) {
      later(
        function() {
          isolate({
            data_loading_trigger(data_loading_trigger() + 1)
          })
        },
        0.3
      )
    }
  })

  # Main gene_data reactive - ONLY loads data, no filtering
  gene_data <- eventReactive(
    list(data_loading_trigger(), input$embedding, input$check_all),
    {
      if (!app_initialized()) {
        return(NULL)
      }

      current_type <- get_current_data_type()
      req(current_type)
      # Force dependency on embedding and integration to ensure plots update
      req(input$embedding)

      # Show progress bar when loading new data
      withProgress(message = "Updating visualization", value = 30, {
        display_name <- switch(current_type,
          raw = "Raw",
          normalize = "Normalized"
          # scale = "Scaled"
        )

        # Load dataset (from cache if available)
        dataset <- load_dataset(current_type)

        result <- list(
          data = dataset,
          genes = genes(dataset, gff_data)
        )

        # Cache the current state
        cached_data$current_type <- current_type
        sessionData$last_gene_data <- result
        return(result)
      })
    },
    ignoreNULL = FALSE
  )

  # Populate filter dropdowns when gene_data changes (once on load)
  observeEvent(gene_data(), {
    req(gene_data()$data)

    filt_data <- gene_data()$data

    # List of filter suffixes for all tabs
    filter_suffixes <- c("", "_gex", "_qc", "_meta")

    # Use isolate to prevent reactive loop
    tryCatch(
      {
        # Populate all filter instances (one for each tab)
        for (suffix in filter_suffixes) {
          updateSelectizeInput(
            session,
            paste0("study_label", suffix),
            choices = unique(filt_data$study_label),
            selected = isolate(input[[paste0("study_label", suffix)]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            paste0("sample_type", suffix),
            choices = unique(filt_data$sample_type),
            selected = isolate(input[[paste0("sample_type", suffix)]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            paste0("host_species", suffix),
            choices = setNames(
              unique(filt_data$host_species),
              format_species_name(unique(filt_data$host_species))
            ),
            selected = isolate(input[[paste0("host_species", suffix)]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            paste0("parasite_stages", suffix),
            choices = unique(filt_data$parasite_stages),
            selected = isolate(input[[paste0("parasite_stages", suffix)]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            paste0("treatment", suffix),
            choices = unique(filt_data$treatment),
            selected = isolate(input[[paste0("treatment", suffix)]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            paste0("development_phase", suffix),
            choices = unique(filt_data$development_phase),
            selected = isolate(input[[paste0("development_phase", suffix)]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            paste0("strain", suffix),
            choices = unique(filt_data$strain),
            selected = isolate(input[[paste0("strain", suffix)]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            paste0("hour_post_invasion", suffix),
            choices = unique(filt_data$hour_post_invasion),
            selected = isolate(input[[paste0("hour_post_invasion", suffix)]]),
            server = TRUE
          )
          # updateSelectizeInput(
          #   session,
          #   paste0("sequencer", suffix),
          #   choices = unique(filt_data$sequencer),
          #   selected = isolate(input[[paste0("sequencer", suffix)]]),
          #   server = TRUE
          # )
          # updateSelectizeInput(
          #   session,
          #   paste0("sc_technology", suffix),
          #   choices = unique(filt_data$sc_technology),
          #   selected = isolate(input[[paste0("sc_technology", suffix)]]),
          #   server = TRUE
          # )
        }
      },
      error = function(e) {
        # Silent error handling
      }
    )
  })

  # Sync filter values across all tabs
  # When a filter changes in any tab, update all other tabs to match
  filter_names <- c(
    "study_label", "sample_type", "host_species", "parasite_stages",
    "treatment", "development_phase", "strain", "hour_post_invasion"
    # "sequencer", "sc_technology"
  )
  filter_suffixes <- c("", "_gex", "_qc", "_meta")

  # Create observers for each filter to sync across tabs
  lapply(filter_names, function(filter_name) {
    lapply(filter_suffixes, function(source_suffix) {
      input_id <- paste0(filter_name, source_suffix)

      observeEvent(input[[input_id]],
        {
          new_value <- input[[input_id]]

          # Update all other instances of this filter
          for (target_suffix in filter_suffixes) {
            if (target_suffix != source_suffix) {
              target_id <- paste0(filter_name, target_suffix)

              # Only update if value is different to avoid loops
              if (!identical(input[[target_id]], new_value)) {
                updateSelectizeInput(
                  session,
                  target_id,
                  selected = new_value
                )
              }
            }
          }
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )
    })
  })


  # Separate reactive for filtered gene data (Embeddings tab)
  filtered_gene_data <- reactive({
    req(gene_data())

    # Collect individual filter inputs (no suffix for Embeddings tab)
    # Empty selections mean "no filter" - show all data
    current_inputs <- list(
      study_label = input$study_label,
      host_species = input$host_species,
      parasite_stages = input$parasite_stages,
      treatment = input$treatment,
      development_phase = input$development_phase,
      sample_type = input$sample_type,
      strain = input$strain,
      hour_post_invasion = input$hour_post_invasion
      # sequencer = input$sequencer,
      # sc_technology = input$sc_technology
    )

    # Create cache key for filtered gene data
    cache_key <- make_cache_key(
      "filtered_gene_data",
      get_current_data_type(),
      current_inputs
    )

    # Check cache first to avoid reprocessing
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }

    filtered_meta <- tryCatch(
      {
        apply_secondary_filters(
          gene_data()$data,
          current_inputs,
          keep_gene_matrix = FALSE,
          genes = if (!is.null(gene_data()$genes)) gene_data()$genes$gene else NULL
        )
      },
      error = function(e) {
        # Return unfiltered data if filtering fails
        gene_data()$data
      }
    )

    # Get row indices if available from the filtered metadata
    row_idx <- attr(filtered_meta, "row_idx")

    # Get the full dataset (with gene expression matrix)
    full_df <- if (!is.null(cached_data[[get_current_data_type()]])) {
      cached_data[[get_current_data_type()]]
    } else {
      gene_data()$data
    }

    result <- list(
      data = filtered_meta,
      full = full_df,
      row_idx = row_idx,
      genes = gene_data()$genes
    )

    # Cache the result
    sessionData[[cache_key]] <- result

    return(result)
  })

  # Synchronize plot controls between tabs on change
  lapply(c("Embeddings", "Gene expression", "Quality control", "Metadata"), function(tab) {
    suffix_map <- list(
      "Embeddings" = "",
      "Gene expression" = "_exp",
      "Quality control" = "_qc",
      "Metadata" = "_meta"
    )
    suffix <- suffix_map[[tab]]

    # Sync data type
    observeEvent(input[[paste0("data_type", suffix)]],
      {
        if (!is.null(input$tabset) && input$tabset == tab && !syncing_tabs()) {
          val <- input[[paste0("data_type", suffix)]]
          for (other_tab in names(suffix_map)) {
            if (other_tab != tab) {
              updateRadioButtons(session, paste0("data_type", suffix_map[[other_tab]]), selected = val)
            }
          }
        }
      },
      ignoreInit = TRUE
    )

    # Sync size
    observeEvent(input[[paste0("size", suffix)]],
      {
        if (!is.null(input$tabset) && input$tabset == tab && !syncing_tabs()) {
          val <- input[[paste0("size", suffix)]]
          for (other_tab in names(suffix_map)) {
            if (other_tab != tab) {
              updateRadioButtons(session, paste0("size", suffix_map[[other_tab]]), selected = val)
            }
          }
        }
      },
      ignoreInit = TRUE
    )

    # Sync plot dimension
    observeEvent(input[[paste0("plot", suffix)]],
      {
        if (!is.null(input$tabset) && input$tabset == tab && !syncing_tabs()) {
          val <- input[[paste0("plot", suffix)]]
          for (other_tab in names(suffix_map)) {
            if (other_tab != tab) {
              updateRadioButtons(session, paste0("plot", suffix_map[[other_tab]]), selected = val)
            }
          }
        }
      },
      ignoreInit = TRUE
    )
  })

  create_data_type_sync_observer(
    input = input,
    session = session,
    input_name = "data_type_exp", # from exp tabset
    target_input = "data_type",
    expected_tabset = "Gene expression",
    global_loading = global_loading
  )

  # Metadata filter
  # choose columns for filter data
  grouped_choices <- list(
    development_phase = setNames(
      paste0("development_phase__", unique(mr_data$development_phase)),
      unique(mr_data$development_phase)
    ),
    # blood_stage = setNames(
    #   paste0("blood_stage__", unique(mr_data$blood_stage)),
    #   unique(mr_data$blood_stage)
    # ),
    parasite_stages = setNames(
      paste0("parasite_stages__", unique(mr_data$parasite_stages)),
      unique(mr_data$parasite_stages)
    ),
    study_label = setNames(
      paste0("study_label__", unique(mr_data$study_label)),
      unique(mr_data$study_label)
    ),
    geographic_location = setNames(
      paste0("geolocation__", unique(mr_data$geographic_location)),
      unique(mr_data$geographic_location)
    ),
    # sample_type = setNames(
    #   paste0("sample_type__", unique(mr_data$sample_type)),
    #   unique(mr_data$sample_type)
    # ),
    # sc_technology = setNames(
    #   paste0("sc_technology__", unique(mr_data$sc_technology)),
    #   unique(mr_data$sc_technology)
    # ),
    # sequencer = setNames(
    #   paste0("sequencer__", unique(mr_data$sequencer)),
    #   unique(mr_data$sequencer)
    # ),
    host_species = setNames(
      paste0("host_species__", unique(mr_data$host_species)),
      format_species_name(unique(mr_data$host_species))
    ),
    treatment = setNames(
      paste0("treatment__", unique(mr_data$treatment)),
      unique(mr_data$treatment)
    )
  )

  # Old apply_filter code removed - now using individual filter inputs
  user_interact <- reactiveVal(FALSE)

  # Reactive for base metadata (no filtering - just returns mr_data)
  # Filtering is now done at the individual tab level using shared filter inputs
  filtered_data <- reactive({
    mr_data
  })


  observeEvent(
    list(
      input$data_opt,
      input$study_label_meta,
      input$host_species_meta,
      input$parasite_stages_meta,
      input$treatment_meta,
      input$development_phase_meta,
      input$sample_type_meta,
      input$strain_meta,
      input$hour_post_invasion_meta
      # input$sequencer_meta,
      # input$sc_technology_meta
    ),
    {
      current_inputs <- list(
        study_label = input$study_label_meta,
        sample_type = input$sample_type_meta,
        host_species = input$host_species_meta,
        parasite_stages = input$parasite_stages_meta,
        treatment = input$treatment_meta,
        development_phase = input$development_phase_meta,
        strain = input$strain_meta,
        hour_post_invasion = input$hour_post_invasion_meta
        # sequencer = input$sequencer_meta,
        # sc_technology = input$sc_technology_meta
      )

      filt_data <- apply_secondary_filters(filtered_data(), current_inputs)

      # Metadata Value Boxes
      output$total_cells_meta <- renderUI({
        value_box(
          title = "Total Cells",
          value = format(nrow(filt_data), big.mark = ","),
          showcase = fontawesome::fa("dna"),
          theme = "primary"
        )
      })

      output$num_studies_meta <- renderUI({
        value_box(
          title = "Studies",
          value = length(unique(filt_data$study_label)),
          showcase = fontawesome::fa("book"),
          theme = "secondary"
        )
      })

      output$num_species_meta <- renderUI({
        value_box(
          title = "Host Species",
          value = length(unique(filt_data$host_species)),
          showcase = fontawesome::fa("microscope"),
          theme = "info"
        )
      })

      output$data_table <- renderDT(
        {
          # Create metadata table with filtered columns and proper formatting
          processed_data <- filt_data %>%
            select(
              -c(
                starts_with("umap_"),
                starts_with("pca_"),
                starts_with("t_sne_"),
                barcode,
                # parasite_stage,
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
                "parasite_stages",
                "development_phase",
                "blood_stage",
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
              Development_phase,
              Parasite_stages,
              Blood_stage,
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

      ## plot sample vs count
      output$sample_plotly <- renderPlotly({
        meta_plotly(filt_data, "sample_type", "Count", "Sample type", "bar")
      })

      ## plot species vs count
      output$species_plotly <- renderPlotly({
        meta_plotly(filt_data, "host_species", "Count", "Host species", "bar")
      })

      ## plot strain vs count
      output$strain_plotly <- renderPlotly({
        meta_plotly(filt_data, "strain", "Count", "Strain", "bar")
      })

      ## plot reproductive_form vs count
      output$treatment_plotly <- renderPlotly({
        meta_plotly(
          filt_data,
          "treatment",
          "Count",
          "Treatment",
          "bar"
        )
      })

      ## plot parasite stages vs count
      output$parasite_stages_plotly <- renderPlotly({
        meta_plotly(
          filt_data,
          "parasite_stages",
          "Count",
          "Parasite stages",
          "bar"
        )
      })

      ## plot hour post infection vs count
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
  )

  # Quality control tabset
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
      input$hour_post_invasion_qc
      # input$sequencer_qc,
      # input$sc_technology_qc
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
        hour_post_invasion = input$hour_post_invasion_qc
        # sequencer = input$sequencer_qc,
        # sc_technology = input$sc_technology_qc
      )

      filt_data <- apply_secondary_filters(filtered_data(), current_inputs)

      # QC Value Boxes
      output$total_cells_qc <- renderUI({
        value_box(
          title = "Total Cells",
          value = format(nrow(filt_data), big.mark = ","),
          showcase = fontawesome::fa("dna"),
          theme = "primary"
        )
      })

      output$mean_rna_qc <- renderUI({
        avg_rna <- if (nrow(filt_data) > 0) round(mean(filt_data$n_count_rna, na.rm = TRUE), 0) else 0
        value_box(
          title = "Mean RNA Counts",
          value = format(avg_rna, big.mark = ","),
          showcase = fontawesome::fa("chart-bar"),
          theme = "secondary"
        )
      })

      output$mean_features_qc <- renderUI({
        avg_feat <- if (nrow(filt_data) > 0) round(mean(filt_data$n_feature_rna, na.rm = TRUE), 0) else 0
        value_box(
          title = "Mean Features",
          value = format(avg_feat, big.mark = ","),
          showcase = fontawesome::fa("fingerprint"),
          theme = "info"
        )
      })

      ## plot nCount in QC tab
      output$data_count_plotly <- renderPlotly({
        feature_plot(
          filt_data,
          x = "study_label",
          y = "n_count_rna",
          color = "development_phase",
          y_title = "RNA count",
          title = "RNA Count"
        )
      })

      ## plot nFeature in QC tab
      output$data_feature_plotly <- renderPlotly({
        feature_plot(
          filt_data,
          x = "study_label",
          y = "n_feature_rna",
          color = "development_phase",
          y_title = "nFeature count",
          title = "Feature Count"
        )
      })

      ## plot elbow
      output$data_pca_stdv_plotly <- renderPlotly({
        selected_study <- if (
          !is.null(input$study_label) && length(input$study_label) > 0
        ) {
          input$study_label[1]
        } else {
          unique(mr_data$study_label)[1]
        }

        # Filter PCA data by selected study
        study_pca_df <- pca_df %>%
          filter(study == selected_study)

        elbow_plot(study_pca_df, selected_study)
      })

      ## plot variable genes
      output$variable_genes_plotly <- renderPlotly({
        selected_study <- if (
          !is.null(input$study_label) && length(input$study_label) > 0
        ) {
          input$study_label[1]
        } else {
          unique(mr_data$study_label)[1]
        }

        var_genes_plot(hiv_data, selected_study, top_n = 15)
      })
    }
  )

  # Data sources tabset
  observeEvent(input$tabset, {
    if (input$tabset == "Data sources") {
      output$data_source <- renderDT({
        DT::datatable(
          pub_data(),
          escape = FALSE,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = FALSE,
            columnDefs = list(
              list(width = "500px", targets = 4, className = "dt-left")
            )
          ),
          selection = "single",
          rownames = FALSE
        )
      })
    }
  })


  # Gene Expression tabset - with optimized caching and debouncing
  filtered_gene_exp_data <- reactive({
    req(gene_data()) # Use raw gene_data() instead of filtered_gene_data()

    current_inputs <- list(
      study_label = input$study_label_gex,
      host_species = input$host_species_gex,
      parasite_stages = input$parasite_stages_gex,
      treatment = input$treatment_gex,
      development_phase = input$development_phase_gex,
      sample_type = input$sample_type_gex,
      strain = input$strain_gex,
      hour_post_invasion = input$hour_post_invasion_gex
      # sequencer = input$sequencer_gex,
      # sc_technology = input$sc_technology_gex
    )

    # Create cache key as a single string for filtered gene expression data
    cache_key <- filtered_gene_exp_cache_key(input)
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }

    tryCatch(
      {
        # Get the full expression matrix from the cached_data storage where
        full_df <- if (!is.null(cached_data[[get_current_data_type()]])) cached_data[[get_current_data_type()]] else gene_data()$data

        filt_meta <- apply_secondary_filters(
          gene_data()$data, # Use raw gene_data() instead of filtered_gene_data()
          current_inputs,
          keep_gene_matrix = FALSE,
          genes = if (!is.null(gene_data()$genes)) gene_data()$genes$gene else NULL
        )

        # Ensure row indices are available on the metadata result
        row_idx <- attr(filt_meta, "row_idx")

        result <- list(
          data = filt_meta,
          genes = gene_data()$genes, # FIXED: Use gene_data() instead of filtered_gene_data()
          full = full_df,
          row_idx = row_idx
        )

        sessionData[[cache_key]] <- result

        return(result)
      },
      error = function(e) {
        return(NULL)
      }
    )
  }) %>% debounce(500)

  observeEvent(
    list(
      input$data_opt,
      input$study_label_gex,
      input$host_species_gex,
      input$parasite_stages_gex,
      input$treatment_gex,
      input$development_phase_gex,
      input$sample_type_gex,
      input$strain_gex,
      input$hour_post_invasion_gex
      # input$sequencer_gex,
      # input$sc_technology_gex
    ),
    {
      current_inputs <- list(
        study_label = input$study_label_gex,
        host_species = input$host_species_gex,
        parasite_stages = input$parasite_stages_gex,
        treatment = input$treatment_gex,
        development_phase = input$development_phase_gex,
        sample_type = input$sample_type_gex,
        strain = input$strain_gex,
        hour_post_invasion = input$hour_post_invasion_gex
        # sequencer = input$sequencer_gex,
        # sc_technology = input$sc_technology_gex
      )

      filt_data <- apply_secondary_filters(filtered_data(), current_inputs)
    }
  )


  # Manipulation tabset "Gene Exp"
  ## gene_exp_0 block with minimal caching for speed
  choice_exp_0 <- reactive({
    req(gene_data()$genes)
    get_gene_choices(gene_data()$genes, input$check_exp_0)
  })

  # df_exp data for genes_exp_0 with simplified reactive approach and debouncing
  df_exp_0 <- reactive({
    req(filtered_gene_exp_data(), input$genes_exp_0, input$group_exp_0)

    # Skip processing during loading states
    if (global_loading()) {
      return(NULL)
    }

    # Create a safe cache key without digest
    cache_key <- make_cache_key(
      "df_exp",
      input$genes_exp_0,
      input$group_exp_0,
      input$check_exp_0,
      get_current_data_type(),
      input$study_label_gex,
      input$host_species_gex,
      input$parasite_stages_gex,
      input$treatment_gex,
      input$development_phase_gex,
      input$sample_type_gex,
      input$strain_gex,
      input$hour_post_invasion_gex
      # input$sequencer_gex,
      # input$sc_technology_gex
    )

    # Check cache first for instant gene switching
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }

    # Compute result efficiently
    # CRITICAL: Check the actual data being passed to df_exp0
    filtered_data <- filtered_gene_exp_data()

    result <- tryCatch(
      {
        df_exp0(
          filtered_gene_exp_data(),
          input$genes_exp_0,
          input$group_exp_0,
          check_exp = input$check_exp_0,
          full_data = filtered_gene_exp_data()$full
        )
      },
      error = function(e) {
        return(NULL)
      }
    )

    return(result)
  }) %>% debounce(300)

  df_gene0 <- reactiveVal(NULL)

  # Initialize dropdown
  observeEvent(
    gene_data(),
    {
      req(gene_data()$genes)

      initial_choices <- get_gene_choices(gene_data()$genes, FALSE)

      # Update dropdown immediately during app launch
      tryCatch(
        {
          updateSelectizeInput(
            session,
            "genes_exp_0",
            label = "Gene name",
            choices = initial_choices,
            selected = initial_choices[1],
            options = list(
              placeholder = "Select one or more genes",
              maxItems = 10
            ),
            server = TRUE
          )
        },
        error = function(e) {
          # Silent error handling
        }
      )
    },
    once = TRUE
  )

  # Update dropdown when check box changes
  observeEvent(input$check_exp_0, {
    req(gene_data()$genes)

    # Get choices based on checkbox state
    choices <- get_gene_choices(gene_data()$genes, input$check_exp_0)

    # Try to preserve current selection if it's still valid in new choices
    current_selection <- input$genes_exp_0
    if (!is.null(current_selection) && all(current_selection %in% choices)) {
      default_selection <- current_selection
    } else {
      # If current selection is not valid, keep first choice
      default_selection <- if (length(choices) > 0) choices[1] else NULL
    }

    tryCatch(
      {
        updateSelectizeInput(
          session,
          "genes_exp_0",
          choices = choices,
          selected = default_selection,
          options = list(
            placeholder = "Select one or more genes",
            maxItems = 10
          )
        )
      },
      error = function(e) {
        # Silent error handling
      }
    )
  })

  # Data summary output for gene expression tab
  output$gene_exp_data_summary <- renderUI({
    # Summary display removed per user request - keeping terminal debug only
    return(HTML(""))
  })

  # Debounced gene selection to prevent flickering
  genes_exp_0_debounced <- reactive({
    input$genes_exp_0
  }) %>% debounce(800)

  group_exp_debounced <- reactive({
    input$group_exp_0
  }) %>% debounce(500)

  # df_exp_0 when the user selects a gene/group - IMPROVED VERSION
  output$gene_exp_plotly_0 <- renderPlotly({
    # Use debounced inputs to prevent flickering
    selected_genes <- genes_exp_0_debounced()
    selected_group <- group_exp_debounced()

    # Validate inputs
    if (is.null(selected_genes) || length(selected_genes) == 0 || is.null(selected_group)) {
      return(create_empty_plot("Please select at least one gene and a grouping option"))
    }

    if (global_loading()) {
      return(create_empty_plot("Loading..."))
    }

    # FIXED: Use filtered_gene_exp_data() which includes ALL filters, not just parasite_stage
    tryCatch(
      {
        # 1. Get the filtered data with ALL filters applied
        filtered_gene_exp <- filtered_gene_exp_data()
        if (is.null(filtered_gene_exp) || is.null(filtered_gene_exp$data)) {
          return(create_empty_plot("Data not loaded"))
        }

        # 2. Use the already filtered data instead of applying only parasite_stage filter
        filtered_data <- filtered_gene_exp$data
        raw_data <- list(
          data = filtered_gene_exp$full,
          genes = filtered_gene_exp$genes
        )

        # 3. Get full expression matrix from the filtered data object
        full_matrix <- filtered_gene_exp$full

        # 4. Use row indices from the filtered metadata if available
        row_idx <- attr(filtered_data, "row_idx")

        # 5. Process each selected gene and collect expression values
        gene_expression_matrix <- list()
        valid_gene_names <- c()

        for (gene_name in selected_genes) {
          gene_id <- NULL
          if (!is.null(raw_data$genes)) {
            gene_match <- raw_data$genes[raw_data$genes$name == gene_name, ]
            if (nrow(gene_match) > 0) {
              gene_id <- gene_match$gene[1]
            }
          }

          if (is.null(gene_id)) {
            next # Skip this gene if not found
          }

          if (!(gene_id %in% colnames(full_matrix))) {
            next # Skip if gene expression not found
          }

          # Get row indices for filtered data
          if (!is.null(row_idx) && length(row_idx) > 0) {
            # Use row indices from filtered metadata (this is the correct approach)
            gene_expression <- full_matrix[[gene_id]][row_idx]
          } else {
            # Fallback: match by rownames (less efficient but safer)
            row_indices <- match(rownames(filtered_data), rownames(full_matrix))
            valid_indices <- !is.na(row_indices)

            if (sum(valid_indices) == 0) {
              next # Skip if no matching data
            }

            # Extract expression values for filtered cells
            gene_expression <- full_matrix[[gene_id]][row_indices[valid_indices]]
          }

          gene_expression_matrix[[gene_name]] <- gene_expression
          valid_gene_names <- c(valid_gene_names, gene_name)
        }

        # Check if we have any valid data
        if (length(gene_expression_matrix) == 0) {
          return(create_empty_plot("No data available for selected genes"))
        }

        # 5. Calculate average expression across all selected genes
        if (length(valid_gene_names) == 1) {
          # Single gene - use expression directly as vector
          avg_expression <- gene_expression_matrix[[1]]
          plot_title <- valid_gene_names[1]
        } else {
          # Multiple genes - calculate mean across genes for each cell
          # Convert to matrix and calculate row means
          expr_matrix <- do.call(cbind, gene_expression_matrix)
          avg_expression <- rowMeans(expr_matrix, na.rm = TRUE)
          plot_title <- paste(
            "Average of", length(valid_gene_names), "genes:",
            paste(valid_gene_names, collapse = ", ")
          )
        }

        # 6. Get group values (make sure they match the expression data length)
        if (!is.null(row_idx) && length(row_idx) > 0) {
          # Use the same row indices for group values to ensure consistency
          group_values <- filtered_data[[selected_group]]
        } else {
          # Fallback: match by rownames
          row_indices <- match(rownames(filtered_data), rownames(full_matrix))
          valid_indices <- !is.na(row_indices)
          group_values <- filtered_data[[selected_group]][valid_indices]
        }

        # Ensure lengths match
        if (length(avg_expression) != length(group_values)) {
          # This should not happen, but handle it gracefully
          min_len <- min(length(avg_expression), length(group_values))
          avg_expression <- avg_expression[1:min_len]
          group_values <- group_values[1:min_len]
        }

        # 7. Create final plot data
        plot_data <- data.frame(
          gene_expr = avg_expression,
          group = group_values,
          stringsAsFactors = FALSE
        )
        names(plot_data)[2] <- selected_group

        # Remove NA values
        plot_data <- plot_data[!is.na(plot_data$gene_expr) & !is.na(plot_data[[selected_group]]), ]

        if (nrow(plot_data) == 0) {
          return(create_empty_plot("No data available for selected filters"))
        }

        # 8. Create the plot with title showing which genes were averaged
        group_col <- tolower(selected_group)
        result <- gene_exp_plot(plot_data, group_col, add_title = plot_title)

        return(result)
      },
      error = function(e) {
        return(create_empty_plot(paste("Error:", e$message)))
      }
    )
  })

  ## gene expression link - updated for multiple genes
  observeEvent(input$genes_exp_0, {
    req(input$genes_exp_0)
    # For multiple genes, use the first one for the link
    first_gene <- if (is.character(input$genes_exp_0) && length(input$genes_exp_0) > 0) {
      input$genes_exp_0[1]
    } else {
      input$genes_exp_0
    }
    gene_id <- get_gene_id(first_gene, gene_data()$genes)
    gene_links(gene_id)
  })

  # Redundant observer removed to prevent gene selection resets

  # gene_exp_i block
  # observe add button click from js
  choice_exp <- reactiveVal()
  counter <- reactiveVal(0)
  observeEvent(input$click_add_btn, {
    btn_id <- input$click_add_btn
    req(btn_id)

    id <- as.numeric(gsub("add_btn_?", "", btn_id))

    # Validate that id is a valid number
    if (is.na(id)) {
      return(NULL)
    }

    shinyjs::hide(paste0("add_btn_", id))

    # Use the filtered gene-expression data for choices so selection respects metadata filters
    req(filtered_gene_exp_data())
    choice_exp(isolate(get_gene_choices(filtered_gene_exp_data()$genes, FALSE)))

    ## add ui block to the right after gene_exp_tar
    if (id == 0) {
      id <- id + 1
      insertUI(
        selector = "#gene_exp_container",
        where = "beforeEnd",
        ui = column(
          width = 6,
          id = paste0("dynamic_col_", id),
          subBlock(id)
        )
      )
      # Capture choices now (reactive) then populate selectize shortly after so UI appears quickly
      selectize_choices <- NULL
      try(
        {
          selectize_choices <- choice_exp()
        },
        silent = TRUE
      )
      later(function() {
        tryCatch(
          {
            if (!is.null(selectize_choices)) {
              # Set default selection to first available choice
              default_selection <- if (length(selectize_choices) > 0) selectize_choices[1] else NULL

              updateSelectizeInput(
                session,
                inputId = paste0("genes_exp_", id),
                choices = selectize_choices,
                selected = default_selection,
                server = TRUE
              )
            }
          },
          error = function(e) {}
        )
      }, 0.25)

      # Add observer for the new switch
      observeEvent(input[[paste0("check_exp_", id)]], {
        is_checked <- input[[paste0("check_exp_", id)]]
        choice_exp(get_gene_choices(gene_data()$genes, is_checked))
        updateSelectizeInput(
          session,
          paste0("genes_exp_", id),
          label = if (is_checked) "Gene ID" else "Gene name",
          choices = choice_exp(),
          server = TRUE
        )
      })

      # Create the renderPlotly immediately (ensures render has access to reactives)
      output[[paste0("gene_exp_plotly_", id)]] <- local({
        plot_id <- id
        renderPlotly({
          # Only proceed if we have data and inputs
          req(
            gene_data(),
            input[[paste0("genes_exp_", plot_id)]],
            input[[paste0("group_exp_", plot_id)]]
          )

          # Skip during loading states
          if (global_loading()) {
            return(NULL)
          }

          tryCatch(
            {
              fg <- filtered_gene_exp_data()
              cur_genes <- isolate(input[[paste0("genes_exp_", plot_id)]])
              cur_group <- isolate(input[[paste0("group_exp_", plot_id)]])
              cur_check <- isolate(input[[paste0("check_exp_", plot_id)]])

              # Handle multiple genes
              if (is.null(cur_genes) || length(cur_genes) == 0) {
                return(create_empty_plot("No genes selected"))
              }

              # Convert gene names to IDs
              gene_ids <- c()
              if (isTRUE(cur_check)) {
                # User entered gene IDs directly
                gene_ids <- cur_genes
              } else {
                # User selected gene names - convert to IDs
                for (gene_name in cur_genes) {
                  gene_id <- get_gene_id(gene_name, fg$genes)
                  if (!is.na(gene_id)) {
                    gene_ids <- c(gene_ids, gene_id)
                  }
                }
              }

              if (length(gene_ids) == 0) {
                return(create_empty_plot("No valid genes found"))
              }

              # Build plot data with average if multiple genes
              static_df <- tryCatch(
                {
                  build_plot_slim(fg$data, fg$full, gene_ids, cur_group)
                },
                error = function(e) {
                  NULL
                }
              )

              if (is.null(static_df) || nrow(static_df) == 0) {
                return(create_empty_plot("No data"))
              }

              # Create title showing gene info
              plot_title <- if (length(gene_ids) == 1) {
                cur_genes[1]
              } else {
                paste("Average of", length(gene_ids), "genes:", paste(cur_genes, collapse = ", "))
              }

              gene_exp_plot(static_df, tolower(cur_group), add_title = plot_title)
            },
            error = function(e) {
              return(NULL)
            }
          )
        })
      })
    } else {
      ## add ui block to button
      id <- id + 1
      newBlock(id, counter, choice_exp(), subBlock, session)
      selectize_choices <- NULL
      try(
        {
          selectize_choices <- choice_exp()
        },
        silent = TRUE
      )
      later(function() {
        tryCatch(
          {
            if (!is.null(selectize_choices)) {
              # Set default selection to first available choice
              default_selection <- if (length(selectize_choices) > 0) selectize_choices[1] else NULL

              updateSelectizeInput(
                session,
                inputId = paste0("genes_exp_", id),
                choices = selectize_choices,
                selected = default_selection,
                server = TRUE
              )
            }
          },
          error = function(e) {}
        )
      }, 0.25)

      # Add observer for the new switch
      observeEvent(input[[paste0("check_exp_", id)]], {
        is_checked <- input[[paste0("check_exp_", id)]]
        choice_exp(get_gene_choices(gene_data()$genes, is_checked))
        updateSelectizeInput(
          session,
          paste0("genes_exp_", id),
          label = if (is_checked) "Gene ID" else "Gene name",
          choices = choice_exp(),
          server = TRUE
        )
      })

      # Create the renderPlotly immediately so it captures reactive environment correctly
      output[[paste0("gene_exp_plotly_", id)]] <- local({
        plot_id <- id
        renderPlotly({
          req(
            gene_data(),
            input[[paste0("genes_exp_", plot_id)]],
            input[[paste0("group_exp_", plot_id)]]
          )

          # Skip during loading states
          if (global_loading()) {
            return(NULL)
          }

          tryCatch(
            {
              fg <- filtered_gene_exp_data()
              cur_genes <- isolate(input[[paste0("genes_exp_", plot_id)]])
              cur_group <- isolate(input[[paste0("group_exp_", plot_id)]])
              cur_check <- isolate(input[[paste0("check_exp_", plot_id)]])

              # Handle multiple gene selection
              if (length(cur_genes) == 0 || any(is.null(cur_genes)) || any(cur_genes == "")) {
                return(create_empty_plot("No genes selected"))
              }

              # Convert gene names to IDs
              gene_ids <- if (isTRUE(cur_check)) {
                cur_genes
              } else {
                sapply(cur_genes, function(gene) get_gene_id(gene, fg$genes), USE.NAMES = FALSE)
              }

              # Remove any invalid gene IDs
              valid_genes <- cur_genes[!is.na(gene_ids) & gene_ids != ""]
              valid_gene_ids <- gene_ids[!is.na(gene_ids) & gene_ids != ""]

              if (length(valid_gene_ids) == 0) {
                return(create_empty_plot("No valid genes found"))
              }

              static_df <- tryCatch(
                {
                  build_plot_slim(fg$data, fg$full, valid_gene_ids, cur_group)
                },
                error = function(e) {
                  NULL
                }
              )

              if (is.null(static_df) || nrow(static_df) == 0) {
                return(create_empty_plot("No data"))
              }

              # Create title showing gene info
              plot_title <- if (length(valid_gene_ids) == 1) {
                valid_genes[1]
              } else {
                paste("Average of", length(valid_gene_ids), "genes:", paste(valid_genes, collapse = ", "))
              }

              gene_exp_plot(static_df, tolower(cur_group), add_title = plot_title)
            },
            error = function(e) {
              return(NULL)
            }
          )
        })
      })
    }
  })

  # observe remove button click from js
  observeEvent(
    input$click_remove_btn,
    {
      rm_btn_id <- input$click_remove_btn$btn_id
      id <- as.numeric(gsub("remove_btn_", "", rm_btn_id))
      ids <- as.numeric(unlist(input$click_remove_btn$ids))
      # Remove server-side output binding to free render closures before removing UI
      try(
        {
          output[[paste0("gene_exp_plotly_", id)]] <- NULL
        },
        silent = TRUE
      )

      # whole dynamic block is removed (fallbacks for robustness)
      try(
        {
          removeUI(selector = paste0("#dynamic_col_", id), multiple = FALSE, immediate = TRUE)
        },
        silent = TRUE
      )

      # server-side removal didn't take effect (ensures the whole block vanishes)
      try(
        {
          shinyjs::runjs(sprintf("$('#dynamic_col_%s').remove();", id))
        },
        silent = TRUE
      )
      remaining_ids <- ids[ids != id]

      for (i in remaining_ids) {
        shinyjs::hide(paste0("add_btn_", i))
      }

      if (length(remaining_ids) == 0) {
        shinyjs::show("add_btn_0")
      } else {
        shinyjs::show(paste0("add_btn_", max(remaining_ids)))
      }
    },
    ignoreNULL = TRUE
  )

  ## update gene_id or name in SelectizeInput when toggle button click
  observeEvent(input$check_exp_0, {
    is_checked <- input$check_exp_0
    choice_exp(get_gene_choices(gene_data()$genes, is_checked))

    updateSelectizeInput(
      session,
      "genes_exp_0",
      label = if (is_checked) "Gene ID" else "Gene name",
      choices = choice_exp(),
      server = TRUE
    )
  })

  observeEvent(list(input$click_toggle_exp), {
    req(input$click_toggle_exp)

    tg_id <- input$click_toggle_exp$id
    id <- as.numeric(gsub("check_exp_", "", tg_id))
    is_checked <- input$click_toggle_exp$value

    choice_exp(get_gene_choices(gene_data()$genes, is_checked))

    updateSelectizeInput(
      session,
      paste0("genes_exp_", id),
      label = if (is_checked) "Gene ID" else "Gene name",
      choices = choice_exp(),
      server = TRUE
    )
  })

  observeEvent(input$change_gene_exp, {
    id <- input$change_gene_exp
    id <- as.numeric(gsub("genes_exp_", "", id))
    current_genes <- input[[paste0("genes_exp_", id)]]

    # For multiple genes, use the first one for the link
    first_gene <- if (is.character(current_genes) && length(current_genes) > 0) {
      current_genes[1]
    } else {
      current_genes
    }

    if (!is.null(first_gene) && first_gene != "") {
      gene_id <- get_gene_id(first_gene, gene_data()$genes)
      gene_links(gene_id)
    }
  })

  ## update plot when data_type_exp_change from js with debouncing
  gene_exp_plots_trigger <- reactiveVal(0)

  choice <- eventReactive(list(input$check, input$data_type), {
    req(gene_data()$genes)
    get_gene_choices(gene_data()$genes, input$check)
  })

  ## dynamic UI for gene selection (RNA_snn commented out)
  shinyjs::hide("gene_snn_ui")
  observe({
    req(input$color) # Ensure input$color is available
    if (input$color == "genes") {
      shinyjs::show("gene_snn_ui")
      shinyjs::show("gene_link_ui")
      shinyjs::show("switch_id")

      # Set loading state when showing gene UI
      genes_loading(TRUE)

      # Reset loading state after a short delay to allow other observers to complete
      later(
        function() {
          genes_loading(FALSE)
        },
        0.5
      )
    } else {
      shinyjs::hide("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
      shinyjs::hide("switch_id")
      genes_loading(FALSE)
    }
  })

  # Separate observer to update gene choices when color changes to genes or toggle changes
  observeEvent(list(input$color, input$check),
    {
      if (!identical(input$color, "genes")) {
        return()
      }

      req(gene_data()$genes)

      # Set loading state to prevent plot updates
      genes_loading(TRUE)

      tryCatch(
        {
          genes_df <- gene_data()$genes
          if (is.null(genes_df) || nrow(genes_df) == 0) {
            genes_loading(FALSE)
            return()
          }

          gene_choices <- get_gene_choices(genes_df, input$check)

          if (is.null(gene_choices) || length(gene_choices) == 0) {
            genes_loading(FALSE)
            return()
          }

          # Store current selection before updating choices
          current_selection <- input$genes_n

          # If color by genes is selected and no gene is chosen, pick a default
          if (is.null(current_selection) || length(current_selection) == 0) {
            current_selection <- if (length(gene_choices) > 0) gene_choices[1] else NULL
          }

          updateSelectizeInput(
            session,
            "genes_n",
            choices = gene_choices,
            selected = current_selection, # Preserve current selection
            options = list(
              placeholder = "Select one or more genes",
              maxItems = 10
            ),
            server = TRUE
          )

          # Update the label text based on toggle state
          label_text <- if (isTRUE(input$check)) "Gene ID" else "Gene name"
          runjs(sprintf('$("#gene_label").text("%s");', label_text))

          # Reset loading state after choices are updated
          later(
            function() {
              genes_loading(FALSE)
            },
            0.1
          )
        },
        error = function(e) {
          message("Error updating gene choices: ", e$message)
          genes_loading(FALSE)
        }
      )
    },
    ignoreInit = FALSE
  )

  ## hide switch for tsne
  # Removed: observeEvent for hiding tab_switch via JS
  # Integration toggle now handled via conditionalPanel in ui.R


  ## get x- and y- axis selections for 2D plots
  observeEvent(
    list(
      input$embedding,
      input$plot,
      input$xaxis,
      input$check_all
    ),
    {
      if (input$plot == "2d") {
        shinyjs::show("axis")
        x_choices <- switch(input$embedding,
          "umap" = if (isTRUE(input$check_all)) umap_i else umap_u,
          "pca" = if (isTRUE(input$check_all)) pca_i else pca_u,
          "tsne" = tsne
        )

        updateSelectInput(
          session,
          "xaxis",
          choices = x_choices,
          selected = x_choices[1]
        )

        y_choices <- switch(input$embedding,
          "umap" = y_col(
            input$check_all,
            input$xaxis,
            umap_i,
            umap_u
          ),
          "pca" = y_col(input$check_all, input$xaxis, pca_i, pca_u),
          "tsne" = setdiff(tsne, input$xaxis)
        )

        updateSelectInput(
          session,
          "yaxis",
          choices = y_choices,
          selected = if (input$yaxis %in% y_choices) {
            input$yaxis
          } else {
            y_choices[2]
          }
        )
      } else {
        shinyjs::hide("axis")
      }
    }
  )

  ## automatically update size selection based on embedding plot 2D or 3D
  observeEvent(
    input$plot,
    {
      if (!is.null(input$plot)) {
        if (input$plot == "2d") {
          updateRadioButtons(session, "size", selected = 2)
          runjs(
            '$("input[name=\'size\'][value=\'2\']").prop("checked", true).trigger("change");'
          )
        } else if (input$plot == "3d") {
          updateRadioButtons(session, "size", selected = 4)
          runjs(
            '$("input[name=\'size\'][value=\'4\']").prop("checked", true).trigger("change");'
          )
        }
      }
    },
    ignoreInit = TRUE
  )

  ## choose data based on input color
  data <- reactive({
    filtered_gene_data()$data
  })

  # Separate data reactive for first plot (used in comparison mode)
  first_plot_data <- reactive({
    req(comparison_mode())

    # Cache the data and gene objects at evaluation time to avoid interference
    tryCatch(
      {
        current_data <- filtered_gene_data()$data
        current_genes <- gene_data()$genes
        current_full_data <- gene_data()$data
        current_data_type <- get_current_data_type()

        # Add cached objects as attributes to prevent cross-plot interference
        attr(current_data, "cached_genes") <- current_genes
        attr(current_data, "cached_full_data") <- current_full_data
        attr(current_data, "cached_data_type") <- current_data_type

        current_data
      },
      error = function(e) {
        NULL
      }
    )
  })

  # Separate data reactive for second plot (used in comparison mode)
  second_plot_data <- reactive({
    req(comparison_mode())

    # Cache the data and gene objects at evaluation time to avoid interference
    tryCatch(
      {
        current_data <- filtered_gene_data()$data
        current_genes <- gene_data()$genes
        current_full_data <- gene_data()$data
        current_data_type <- get_current_data_type()

        # Add cached objects as attributes to prevent cross-plot interference
        attr(current_data, "cached_genes") <- current_genes
        attr(current_data, "cached_full_data") <- current_full_data
        attr(current_data, "cached_data_type") <- current_data_type

        current_data
      },
      error = function(e) {
        NULL
      }
    )
  })

  # Observer to update gene choices for first plot in comparison mode
  observeEvent(list(input$color_first_comp, input$check_first_comp), {
    req(isTRUE(comparison_mode()), identical(input$color_first_comp, "genes"), gene_data()$genes)
    tryCatch(
      {
        choices <- get_gene_choices(gene_data()$genes, isTRUE(input$check_first_comp))
        updateSelectizeInput(session, "genes_n_first_comp", choices = choices, selected = input$genes_n_first_comp, server = TRUE)
      },
      error = function(e) {}
    )
  })

  # Create reactive expressions for each plot that only depend on their specific inputs
  first_plot_reactive <- reactive({
    req(comparison_mode())
    # Ensure axes are ready if in 2D mode
    if (isTRUE(input$plot == "2d")) {
      req(input$xaxis, input$yaxis)
    }

    # Determine which inputs to use based on comparison mode
    color_val <- if (!is.null(input$color_first_comp)) input$color_first_comp else "development_phase"
    genes_val <- if (!is.null(input$genes_n_first_comp)) input$genes_n_first_comp else ""
    check_val <- if (!is.null(input$check_first_comp)) input$check_first_comp else FALSE

    list(
      color = color_val,
      genes = genes_val,
      check = check_val,
      xaxis = input$xaxis,
      yaxis = input$yaxis,
      size = if (!is.null(input$size)) input$size else 4,
      plot = if (!is.null(input$plot)) input$plot else "3d",
      embedding = input$embedding,
      integration = input$check_all,
      timestamp = Sys.time()
    )
  })

  # Reactive expression for second plot parameters
  second_plot_reactive <- reactive({
    req(comparison_mode())
    if (isTRUE(input$plot == "2d")) {
      req(input$xaxis, input$yaxis)
    }

    color_val <- if (!is.null(input$color_second)) input$color_second else "development_phase"
    genes_val <- if (!is.null(input$genes_second)) input$genes_second else ""
    check_val <- if (!is.null(input$check_second)) input$check_second else FALSE

    list(
      color = color_val,
      genes = genes_val,
      check = check_val,
      xaxis = input$xaxis,
      yaxis = input$yaxis,
      size = if (!is.null(input$size)) input$size else 4,
      plot = if (!is.null(input$plot)) input$plot else "3d",
      embedding = input$embedding,
      integration = input$check_all,
      timestamp = Sys.time()
    )
  })

  # Update plot subtitles based on selected color option
  observe({
    req(comparison_mode())

    color_map <- list(
      study_label = "Study",
      development_phase = "Development Phase",
      parasite_stages = "Parasite Stages",
      strain = "Strain",
      treatment = "Treatment",
      sample_type = "Sample Type",
      host_species = "Host Species",
      genes = "Genes"
    )

    plot_a_label <- color_map[[input$color_first_comp]] %||% "Development Phase"
    plot_b_label <- color_map[[input$color_second]] %||% "Parasite Stages"

    shinyjs::html("plot_a_subtitle", plot_a_label)
    shinyjs::html("plot_b_subtitle", plot_b_label)
  })

  # Add debouncing for plot updates
  plot_update_trigger <- reactiveVal(0)
  plot_ready <- reactiveVal(TRUE)
  genes_loading <- reactiveVal(FALSE)
  autoUpdatePlot <- reactiveVal(TRUE)

  # Simplified debounced observer for plot-related inputs
  observe({
    list(
      input$embedding,
      input$plot,
      input$xaxis,
      input$yaxis,
      input$color,
      input$size,
      input$genes_n,
      input$data_type,
      input$check,
      input$check_all,
      input$study_label,
      input$host_species,
      input$parasite_stages,
      input$treatment,
      input$development_phase,
      input$sample_type,
      input$strain,
      input$hour_post_invasion,
      input$sequencer,
      input$sc_technology
    )

    # Set plot as not ready immediately when inputs change
    tryCatch(
      {
        plot_ready(FALSE)
        autoUpdatePlot(FALSE)
      },
      error = function(e) {}
    )

    # Simple debounce - wait for inputs to stabilize
    later(
      function() {
        tryCatch(
          {
            isolate({
              plot_update_trigger(plot_update_trigger() + 1)
              plot_ready(TRUE)
              autoUpdatePlot(TRUE)
            })
          },
          error = function(e) {
            plot_ready(TRUE)
            autoUpdatePlot(TRUE)
          }
        )
      },
      0.6
    )
  })

  ## determine plot type
  get_plot_type <- reactive({
    emb <- if (!is.null(input$embedding)) input$embedding else "umap"
    switch(emb,
      "pca" = if (isTRUE(input$check_all)) "pca_i" else "pca_u",
      "umap" = if (isTRUE(input$check_all)) "umap_i" else "umap_u",
      "tsne" = "tSNE",
      "umap_i" # default
    )
  })

  ## generate pca, umap and tsne plot with comprehensive dependency checking
  embplot <- reactiveVal(FALSE)

  # Embedding plot readiness state
  embedding_ready <- reactiveVal(FALSE)
  last_gene_selection <- reactiveVal("")

  # Comprehensive dependency checker with gene selection stability
  all_dependencies_ready <- reactive({
    # Core data requirements - safely check data()
    data_result <- try(data(), silent = TRUE)
    if (inherits(data_result, "try-error") || is.null(data_result) || nrow(data_result) == 0) {
      return(FALSE)
    }

    # Basic plot settings
    if (is.null(input$xaxis) || is.null(input$yaxis)) {
      return(FALSE)
    }

    # Plot readiness and auto-update flags
    if (!plot_ready() || !autoUpdatePlot()) {
      return(FALSE)
    }

    # For gene coloring, ensure stable gene selection
    if (input$color == "genes") {
      current_gene <- input$genes_n
      if (is.null(current_gene) || length(current_gene) == 0 || any(current_gene == "")) {
        return(FALSE)
      }

      # Check if gene selection has been stable
      if (!identical(last_gene_selection(), current_gene)) {
        last_gene_selection(current_gene)
        # Give a moment for the gene selection to stabilize
        invalidateLater(150)
        return(FALSE)
      }
    }
    return(TRUE)
  })

  # Observer to set ready state when dependencies are met
  observe({
    ready_state <- try(all_dependencies_ready(), silent = TRUE)
    if (!inherits(ready_state, "try-error")) {
      embedding_ready(ready_state)
    }
  })


  # Helper function to generate embedding plots with different color options
  generate_embedding_plot <- function(color_input, genes_input, check_input, xaxis_input, yaxis_input, size_input, plot_input, plot_type_input, is_dual_plot = FALSE, data_source = NULL) {
    # For dual plot mode, skip the global dependency checks and use simpler validation
    if (is_dual_plot) {
      # Basic data validation for comparison mode
      plot_data <- if (!is.null(data_source)) data_source else data()
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(NULL)
      }

      # For gene coloring in dual plot mode, check the specific gene inputs
      if (color_input == "genes") {
        if (is.null(genes_input) || length(genes_input) == 0 || any(is.na(genes_input)) || any(genes_input == "")) {
          return(NULL)
        }
      }
    } else {
      # Single plot mode - use the existing dependency system
      req(all_dependencies_ready(), embedding_ready())
    }

    # For single plot mode, get data after dependency checks
    if (!is_dual_plot) {
      plot_data <- if (!is.null(data_source)) data_source else data()
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(NULL)
      }
    }

    # Generate plot without progress bar
    isolate({
      # Work on a local copy so we can attach a temporary gene-expression column
      df_local <- plot_data

      color_column <- NULL

      if (isTRUE(color_input == "genes")) {
        gene_inputs <- genes_input

        # Handle empty or invalid gene inputs by falling back to sample_type
        if (is.null(gene_inputs) || length(gene_inputs) == 0 ||
          (length(gene_inputs) == 1 && (is.na(gene_inputs) || gene_inputs == ""))) {
          color_column <- "development_phase"
        } else {
          # For dual plot mode, use cached data to prevent interference between plots
          if (is_dual_plot && !is.null(data_source)) {
            gene_data_obj <- attr(data_source, "cached_genes")
            full_mat <- attr(data_source, "cached_full_data")

            # Fallback if cached data is missing
            if (is.null(gene_data_obj)) {
              gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
            }
            if (is.null(full_mat)) {
              full_mat <- tryCatch(gene_data()$data, error = function(e) NULL)
            }
          } else {
            # Single plot mode - use dynamic data
            gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
            full_mat <- if (!is.null(cached_data[[get_current_data_type()]])) cached_data[[get_current_data_type()]] else gene_data()$data
          }

          row_idx <- attr(df_local, "row_idx")

          resolved_gene_col <- NULL

          # Handle multiple gene selection
          if (!is.null(gene_inputs) && length(gene_inputs) > 0) {
            if (length(gene_inputs) > 1) {
              # For multiple genes, calculate average expression
              valid_gene_ids <- c()

              for (gene_input in gene_inputs) {
                current_resolved <- NULL

                # If the UI 'check' toggle indicates the user entered gene ids, try that first
                if (!is.null(gene_input) && nzchar(as.character(gene_input)) && isTRUE(check_input)) {
                  gene_id_from_input <- as.character(gene_input)

                  # Directly check if this gene ID exists in our data matrix
                  if (gene_id_from_input %in% colnames(full_mat)) {
                    current_resolved <- gene_id_from_input
                  }
                }

                # If we didn't resolve by gene ID, try by gene name
                if (is.null(current_resolved) && !is.null(gene_data_obj)) {
                  tryCatch(
                    {
                      candidate_id <- get_gene_id(as.character(gene_input), gene_data_obj)
                      if (!is.null(candidate_id) && !is.na(candidate_id) && candidate_id %in% colnames(full_mat)) {
                        current_resolved <- candidate_id
                      }
                    },
                    error = function(e) {
                      # Silently continue if gene lookup fails
                    }
                  )
                }

                # Final fallback: direct string match in column names
                if (is.null(current_resolved)) {
                  gene_as_string <- as.character(gene_input)
                  if (gene_as_string %in% colnames(full_mat)) {
                    current_resolved <- gene_as_string
                  }
                }

                if (!is.null(current_resolved)) {
                  valid_gene_ids <- c(valid_gene_ids, current_resolved)
                }
              }

              # Calculate average if we have multiple valid genes
              if (length(valid_gene_ids) > 1) {
                if (!is.null(row_idx) && length(row_idx) > 0) {
                  # Average the selected genes for the filtered rows
                  gene_matrix <- full_mat[row_idx, valid_gene_ids, drop = FALSE]
                  avg_expression <- rowMeans(gene_matrix, na.rm = TRUE)
                  df_local$multi_gene_avg <- avg_expression
                  resolved_gene_col <- "multi_gene_avg"
                }
              } else if (length(valid_gene_ids) == 1) {
                # Single valid gene from multi-gene selection - attach expression values
                gene_id <- valid_gene_ids[1]
                if (!is.null(row_idx) && length(row_idx) > 0) {
                  df_local[[gene_id]] <- full_mat[row_idx, gene_id]
                } else {
                  df_local[[gene_id]] <- full_mat[, gene_id]
                }
                resolved_gene_col <- gene_id
              }
            } else {
              # Single gene selection
              gene_input <- gene_inputs[1]

              # If the UI 'check' toggle indicates the user entered a gene id, try that first
              if (!is.null(gene_input) && nzchar(as.character(gene_input)) && isTRUE(check_input)) {
                gene_id_from_input <- as.character(gene_input)

                # Directly check if this gene ID exists in our data matrix
                if (gene_id_from_input %in% colnames(full_mat)) {
                  resolved_gene_col <- gene_id_from_input
                }
              }

              # If we didn't resolve by gene ID, try by gene name
              if (is.null(resolved_gene_col) && !is.null(gene_data_obj)) {
                tryCatch(
                  {
                    candidate_id <- get_gene_id(as.character(gene_input), gene_data_obj)
                    if (!is.null(candidate_id) && !is.na(candidate_id) && candidate_id %in% colnames(full_mat)) {
                      resolved_gene_col <- candidate_id
                    }
                  },
                  error = function(e) {
                    # Silently continue if gene lookup fails
                  }
                )
              }

              # Final fallback: direct string match in column names
              if (is.null(resolved_gene_col)) {
                gene_as_string <- as.character(gene_input)
                if (gene_as_string %in% colnames(full_mat)) {
                  resolved_gene_col <- gene_as_string
                }
              }
            }
          }

          if (!is.null(resolved_gene_col)) {
            if (resolved_gene_col != "multi_gene_avg" && resolved_gene_col %in% colnames(full_mat)) {
              if (!is.null(row_idx) && length(row_idx) > 0) {
                # Attach gene expression values for the filtered rows
                df_local[[resolved_gene_col]] <- full_mat[row_idx, resolved_gene_col]
              } else {
                # If no row_idx, use all rows
                df_local[[resolved_gene_col]] <- full_mat[, resolved_gene_col]
              }
            }

            # Now verify the gene column exists in df_local
            if (resolved_gene_col %in% colnames(df_local)) {
              color_column <- resolved_gene_col
            } else {
              # Gene was resolved but doesn't exist in current data
              return(NULL)
            }
          } else {
            # No valid gene found
            return(NULL)
          }
        } # End of gene processing block
      } else {
        # For non-gene colors, use the column directly
        color_column <- color_input
      }

      if (is.null(color_column)) {
        return(NULL)
      }

      # Determine proper legend title for gene expression
      effective_legend_title <- if (isTRUE(color_input == "genes")) {
        if (!is.null(genes_input) && length(genes_input) > 0) {
          if (length(genes_input) == 1) genes_input[1] else paste0("Avg of ", length(genes_input), " genes")
        } else {
          "Gene expression"
        }
      } else {
        color_input
      }

      # Generate the actual plot
      p <- embedding_plot(
        df_local,
        xaxis_input,
        yaxis_input,
        color_column,
        effective_legend_title,
        size_input,
        plot_type = plot_type_input,
        plot_dim = plot_input,
        is_dual_plot = is_dual_plot
      )
      p <- p %>%
        event_register("plotly_click") %>%
        config(
          displaylogo = FALSE,
          scrollZoom = TRUE,
          modeBarButtonsToRemove = c(),
          displayModeBar = TRUE
        )

      return(p)
    })
  }

  # Handle Compare Plots button click (toggle comparison mode)
  observeEvent(input$enable_comparison, {
    new_state <- !comparison_mode()
    comparison_mode(new_state)

    # Update button appearance
    if (new_state) {
      updateActionButton(session, "enable_comparison",
        label = tagList(fontawesome::fa("columns"), " Single Plot"),
        icon = NULL
      )
      shinyjs::addClass("enable_comparison", "btn-primary")
      shinyjs::removeClass("enable_comparison", "btn-outline-primary")

      # Show comparison panels, hide single plot
      shinyjs::show("comparison_controls_panel")
      shinyjs::show("comparison_plots_panel")
      shinyjs::hide("single_plot_panel")
      shinyjs::hide("color_by_single")
      shinyjs::show("comparison_mode_badge")

      # Store current selections
      ui_state$stored_color <- input$color
      ui_state$stored_genes <- input$genes_n
      ui_state$stored_check <- input$check

      # Initialize comparison plots with same settings as main plot
      updateSelectInput(session, "color_first_comp", selected = input$color)
      updateSelectInput(session, "color_second", selected = input$color)

      # If using genes, copy gene selection
      if (!is.null(input$color) && input$color == "genes" && !is.null(input$genes_n) && length(input$genes_n) > 0) {
        current_genes <- input$genes_n
        later(function() {
          updateSelectizeInput(session, "genes_n_first_comp", selected = current_genes)
          updateSelectizeInput(session, "genes_second", selected = current_genes)
        }, 0.2)
      }
    } else {
      updateActionButton(session, "enable_comparison",
        label = tagList(fontawesome::fa("columns"), " Compare Plots"),
        icon = NULL
      )
      shinyjs::removeClass("enable_comparison", "btn-primary")
      shinyjs::addClass("enable_comparison", "btn-outline-primary")

      # Hide comparison panels, show single plot
      shinyjs::hide("comparison_controls_panel")
      shinyjs::hide("comparison_plots_panel")
      shinyjs::show("single_plot_panel")
      shinyjs::show("color_by_single")
      shinyjs::hide("comparison_mode_badge")

      # Restore previous selections
      if (!is.null(ui_state$stored_color)) {
        updateSelectInput(session, "color", selected = ui_state$stored_color)
      }
    }
  })

  # Continuously update stored state when user makes changes (only when comparison is disabled)
  observe({
    if (!comparison_mode()) {
      ui_state$stored_color <- input$color
      ui_state$stored_genes <- input$genes_n
      ui_state$stored_check <- input$check
    }
  })

  # Handle genes panel visibility for single plot mode
  observe({
    if (!comparison_mode() && !is.null(input$color) && input$color == "genes") {
      shinyjs::show("genes_single_panel")
    } else {
      shinyjs::hide("genes_single_panel")
    }
  })

  # Separate observer for second plot color
  observe({
    # Only update second plot label if comparison is enabled
    if (isTRUE(comparison_mode())) {
      req(input$color_second)

      # Map color values to display names
      color_names <- c(
        "study_label" = "Study",
        "development_phase" = "Development phase",
        # "blood_stage" = "Blood stage",
        "parasite_stages" = "Parasite stage",
        "strain" = "Strain",
        "treatment" = "Treatment",
        "sample_type" = "Sample type",
        "host_species" = "Host species",
        # "sequencer" = "Sequencer",
        # "sc_technology" = "SC technology",
        # "day_post_infection" = "DPI",
        # "hour_post_invasion" = "HPI",
        "genes" = "Genes"
      )

      second_label <- if (!is.null(color_names[input$color_second])) color_names[input$color_second] else "Unknown"
      runjs(paste0("$('#second_plot_label').text('Second Color: ", second_label, "');"))
    }
  })

  # Update gene choices for second plot when comparison mode and color_second changes
  observeEvent(list(input$color_second, input$check_second),
    {
      if (!isTRUE(comparison_mode()) || !identical(input$color_second, "genes")) {
        return()
      }

      req(gene_data()$genes)

      tryCatch(
        {
          genes_df <- gene_data()$genes
          if (is.null(genes_df) || nrow(genes_df) == 0) {
            return()
          }

          gene_choices <- get_gene_choices(genes_df, isTRUE(input$check_second))

          if (is.null(gene_choices) || length(gene_choices) == 0) {
            return()
          }

          # Store current selection before updating choices
          current_selection_second <- input$genes_second

          updateSelectizeInput(
            session,
            "genes_second",
            choices = gene_choices,
            selected = current_selection_second, # Preserve current selection
            options = list(
              placeholder = "Select one or more genes",
              maxItems = 10
            ),
            server = TRUE
          )

          # Update the label text based on toggle state
          label_text_second <- if (isTRUE(input$check_second)) "Gene ID" else "Gene name"
          runjs(sprintf('$("#gene_label_second").text("%s");', label_text_second))
        },
        error = function(e) {
          message("Error updating second plot gene choices: ", e$message)
        }
      )
    },
    ignoreInit = FALSE
  )

  # Helper function to validate gene selection
  validate_gene_selection <- function(genes_input, plot_name = "plot") {
    if (is.null(genes_input)) {
      return(FALSE)
    }

    if (length(genes_input) == 0) {
      return(FALSE)
    }

    # Check if all gene inputs are non-empty strings
    if (any(is.na(genes_input)) || any(genes_input == "")) {
      return(FALSE)
    }

    return(TRUE)
  }

  second_plot_reactive <- reactive({
    req(comparison_mode())

    # Get current values with defaults
    color_second <- if (!is.null(input$color_second)) input$color_second else "sample_type"
    genes_second <- if (!is.null(input$genes_second)) input$genes_second else ""
    check_second <- if (!is.null(input$check_second)) input$check_second else FALSE
    # Use main plot axes, size and dimension for second plot
    xaxis_val <- if (!is.null(input$xaxis)) input$xaxis else "pc_1"
    yaxis_val <- if (!is.null(input$yaxis)) input$yaxis else "pc_2"
    size_val <- if (!is.null(input$size)) input$size else 4
    plot_val <- if (!is.null(input$plot)) input$plot else "3d"

    # For gene coloring, allow empty genes and let the plot function handle it
    if (color_second == "genes") {
      if (is.null(genes_second) || (length(genes_second) == 1 && genes_second == "")) {
        genes_second <- ""
      }
    }

    list(
      color = color_second,
      genes = genes_second,
      check = check_second,
      xaxis = xaxis_val,
      yaxis = yaxis_val,
      size = size_val,
      plot = plot_val,
      timestamp = Sys.time()
    )
  })

  # Observer for first plot gene links (during comparison)
  observe({
    req(comparison_mode(), input$color_first_comp == "genes", input$genes_n_first_comp)

    # Get all selected genes for links
    selected_genes <- input$genes_n_first_comp
    if (!is.null(selected_genes) && length(selected_genes) > 0) {
      gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
      if (!is.null(gene_data_obj)) {
        # Get gene IDs for all selected genes
        gene_ids <- sapply(selected_genes, function(gene) {
          tryCatch(get_gene_id(gene, gene_data_obj), error = function(e) gene)
        })
        gene_ids <- gene_ids[!is.null(gene_ids) & nzchar(gene_ids)]
        if (length(gene_ids) > 0) {
          gene_links_multiple(gene_ids, target = "plasmo_link_first")
        }
      }
    }
  })

  # Observer for second plot gene links
  observe({
    req(comparison_mode(), input$color_second == "genes", input$genes_second)

    # Get all selected genes for links
    selected_genes <- input$genes_second
    if (!is.null(selected_genes) && length(selected_genes) > 0) {
      gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
      if (!is.null(gene_data_obj)) {
        # Get gene IDs for all selected genes
        gene_ids <- sapply(selected_genes, function(gene) {
          tryCatch(get_gene_id(gene, gene_data_obj), error = function(e) gene)
        })
        gene_ids <- gene_ids[!is.null(gene_ids) & nzchar(gene_ids)]
        if (length(gene_ids) > 0) {
          gene_links_multiple(gene_ids, target = "plasmo_link_second")
        }
      }
    }
  })

  # Observer for single plot gene links (when comparison is disabled)
  observe({
    req(!isTRUE(comparison_mode()), input$color == "genes", input$genes_n)

    # Get all selected genes for links
    selected_genes <- input$genes_n
    if (!is.null(selected_genes) && length(selected_genes) > 0) {
      gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
      if (!is.null(gene_data_obj)) {
        # Get gene IDs for all selected genes
        gene_ids <- sapply(selected_genes, function(gene) {
          tryCatch(get_gene_id(gene, gene_data_obj), error = function(e) gene)
        })
        gene_ids <- gene_ids[!is.null(gene_ids) & nzchar(gene_ids)]
        if (length(gene_ids) > 0) {
          gene_links_multiple(gene_ids)
        }
      }
    }
  })

  # First plot output (uses main color control - same as single plot)
  output$first_plotly <- renderPlotly({
    # Get plot parameters
    plot_params <- first_plot_reactive()

    # If plot_params is NULL (e.g., during initialization), return NULL to show spinner
    if (is.null(plot_params)) {
      return(NULL)
    }

    generate_embedding_plot(
      plot_params$color,
      plot_params$genes,
      plot_params$check,
      plot_params$xaxis,
      plot_params$yaxis,
      plot_params$size,
      plot_params$plot,
      plot_type_input = get_plot_type(),
      is_dual_plot = TRUE,
      data_source = first_plot_data()
    )
  })

  # Second plot output (uses second color control)
  output$second_plotly <- renderPlotly({
    # Get plot parameters
    plot_params <- second_plot_reactive()

    # If plot_params is NULL (e.g., during initialization), return NULL to show spinner
    if (is.null(plot_params)) {
      return(NULL)
    }

    generate_embedding_plot(
      plot_params$color,
      plot_params$genes,
      plot_params$check,
      plot_params$xaxis,
      plot_params$yaxis,
      plot_params$size,
      plot_params$plot,
      plot_type_input = get_plot_type(),
      is_dual_plot = TRUE,
      data_source = second_plot_data()
    )
  })

  # Keep the original plot for backward compatibility
  output$all_plotly <- renderPlotly({
    # Only render when comparison is NOT enabled
    req(!isTRUE(comparison_mode()))
    req(all_dependencies_ready(), embedding_ready())

    # Triple-check data is still valid
    if (is.null(data()) || nrow(data()) == 0) {
      return(NULL)
    }

    # Determine plot type outside isolate to add dependency
    ptype <- get_plot_type()

    # Generate plot without progress bar
    isolate({
      df_local <- data()

      color_column <- NULL

      if (isTRUE(input$color == "genes")) {
        # Resolve gene input robustly: user may supply display names or gene ids (multiple selection supported)
        gene_inputs <- input$genes_n
        gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
        full_mat <- if (!is.null(cached_data[[get_current_data_type()]])) cached_data[[get_current_data_type()]] else gene_data()$data
        row_idx <- attr(df_local, "row_idx")

        resolved_gene_col <- NULL

        # Handle multiple gene selection
        if (!is.null(gene_inputs) && length(gene_inputs) > 0) {
          if (length(gene_inputs) > 1) {
            # For multiple genes, calculate average expression
            valid_gene_ids <- c()

            for (gene_input in gene_inputs) {
              current_resolved <- NULL

              # If the UI 'check' toggle indicates the user entered gene ids, try that first
              if (!is.null(gene_input) && nzchar(as.character(gene_input)) && isTRUE(input$check)) {
                candidate <- as.character(gene_input)
                if (!is.null(full_mat) && candidate %in% colnames(full_mat)) {
                  current_resolved <- candidate
                }
              }

              # If still not resolved, try mapping display name -> gene id using helper
              if (is.null(current_resolved) && !is.null(gene_input) && nzchar(as.character(gene_input))) {
                mapped <- tryCatch(get_gene_id(as.character(gene_input), gene_data_obj), error = function(e) NULL)
                if (!is.null(mapped) && !is.na(mapped) && !is.null(full_mat) && mapped %in% colnames(full_mat)) {
                  current_resolved <- as.character(mapped)
                }
              }

              # Final fallback: maybe the input text itself is the column name in the matrix
              if (is.null(current_resolved) && !is.null(gene_input) && !is.null(full_mat) && as.character(gene_input) %in% colnames(full_mat)) {
                current_resolved <- as.character(gene_input)
              }

              if (!is.null(current_resolved)) {
                valid_gene_ids <- c(valid_gene_ids, current_resolved)
              }
            }

            # Calculate average expression if we have valid genes
            if (length(valid_gene_ids) > 0 && !is.null(row_idx) && length(row_idx) > 0) {
              gene_matrices <- list()
              for (gene_id in valid_gene_ids) {
                gene_matrices[[gene_id]] <- full_mat[[gene_id]][row_idx]
              }

              if (length(gene_matrices) > 0) {
                gene_matrix <- do.call(cbind, gene_matrices)
                avg_expression <- rowMeans(gene_matrix, na.rm = TRUE)
                gene_color_col <- paste0(".gene_color_avg_", length(valid_gene_ids), "_genes")
                df_local[[gene_color_col]] <- avg_expression
                resolved_gene_col <- gene_color_col
              }
            }
          } else {
            # Single gene - use existing logic
            gene_input <- gene_inputs[1]

            # If the UI 'check' toggle indicates the user entered a gene id, try that first
            if (!is.null(gene_input) && nzchar(as.character(gene_input)) && isTRUE(input$check)) {
              candidate <- as.character(gene_input)
              if (!is.null(full_mat) && candidate %in% colnames(full_mat)) {
                resolved_gene_col <- candidate
              }
            }

            # If still not resolved, try mapping display name -> gene id using helper
            if (is.null(resolved_gene_col) && !is.null(gene_input) && nzchar(as.character(gene_input))) {
              mapped <- tryCatch(get_gene_id(as.character(gene_input), gene_data_obj), error = function(e) NULL)
              if (!is.null(mapped) && !is.na(mapped) && !is.null(full_mat) && mapped %in% colnames(full_mat)) {
                resolved_gene_col <- as.character(mapped)
              }
            }

            # Final fallback: maybe the input text itself is the column name in the matrix
            if (is.null(resolved_gene_col) && !is.null(gene_input) && !is.null(full_mat) && as.character(gene_input) %in% colnames(full_mat)) {
              resolved_gene_col <- as.character(gene_input)
            }

            # Attach single gene expression column
            if (!is.null(resolved_gene_col) && !is.na(resolved_gene_col) && !is.null(full_mat) && resolved_gene_col %in% colnames(full_mat) && !is.null(row_idx) && length(row_idx) > 0) {
              gene_color_col <- paste0(".gene_color_", gsub("[^A-Za-z0-9]", "_", resolved_gene_col))
              df_local[[gene_color_col]] <- full_mat[[resolved_gene_col]][row_idx]
              resolved_gene_col <- gene_color_col
            }
          }
        }

        # Set color column
        if (!is.null(resolved_gene_col) && resolved_gene_col %in% colnames(df_local)) {
          color_column <- resolved_gene_col
        } else {
          # If gene resolution failed, default to development_phase instead of returning NULL
          color_column <- "development_phase"
        }
      } else {
        color_column <- input$color
      }

      # Final safety check: ensure the resolved color column exists in df_local
      if (is.null(color_column) || is.na(color_column) || color_column == "" || !(color_column %in% colnames(df_local))) {
        color_column <- "development_phase"
      }

      # Determine proper legend title for gene expression
      effective_legend_title <- if (isTRUE(input$color == "genes")) {
        if (!is.null(input$genes_n) && length(input$genes_n) > 0) {
          if (length(input$genes_n) == 1) input$genes_n[1] else paste0("Avg of ", length(input$genes_n), " genes")
        } else {
          "Gene expression"
        }
      } else {
        input$color
      }

      p <- embedding_plot(
        df_local,
        input$xaxis,
        input$yaxis,
        color_column,
        effective_legend_title,
        input$size,
        plot_type = ptype,
        plot_dim = input$plot
      )

      p <- p %>%
        event_register("plotly_click") %>%
        config(
          displaylogo = FALSE,
          scrollZoom = TRUE,
          modeBarButtonsToRemove = c(),
          displayModeBar = TRUE
        )

      embplot(TRUE)
      return(p)
    })
  })

  # Add a loading state output for the plot
  output$plot_loading_state <- reactive({
    !all_dependencies_ready()
  })
  outputOptions(output, "plot_loading_state", suspendWhenHidden = FALSE)

  ## click event: after plot is ready
  click_event <- reactive({
    req(embplot())
    ed <- event_data("plotly_click", source = "embedding")
    if (is.null(ed) || is.null(ed$key)) {
      return(NULL)
    }
    return(ed)
  })

  ## table on cell click with memory management
  selected_table <- reactiveVal(NULL)
  observeEvent(click_event(), {
    event <- click_event()
    req(event)

    meta_df <- filtered_gene_data()$data
    full_mat <- gene_data()$data
    cell_key <- event$key

    # Determine available gene columns from the full matrix
    gene_cols <- colnames(full_mat)[str_detect(colnames(full_mat), "^PVP01")]

    # Map the plotly key back to a full-matrix row index
    mapped_full_row <- integer(0)
    try(
      {
        numeric_id <- suppressWarnings(as.integer(as.character(cell_key)))
        if (!is.na(numeric_id) && !is.null(meta_df) && numeric_id >= 1 && numeric_id <= nrow(meta_df)) {
          ridx_attr <- attr(meta_df, "row_idx")
          if (!is.null(ridx_attr) && length(ridx_attr) >= numeric_id) {
            mapped_full_row <- ridx_attr[numeric_id]
          }
        } else if (!is.null(cell_key) && !is.null(meta_df) && any(cell_key == meta_df$barcode, na.rm = TRUE)) {
          # map using metadata barcode -> row_idx
          which_meta <- which(meta_df$barcode == cell_key)[1]
          ridx_attr <- attr(meta_df, "row_idx")
          if (!is.null(which_meta) && !is.null(ridx_attr) && length(ridx_attr) >= which_meta) {
            mapped_full_row <- ridx_attr[which_meta]
          }
        } else if (!is.null(cell_key) && any(cell_key == full_mat$barcode, na.rm = TRUE)) {
          # fallback: match directly in the full matrix's barcode column
          mapped_full_row <- which(full_mat$barcode == cell_key)[1]
        }
      },
      silent = TRUE
    )

    # If we couldn't find a matching full row, return an empty table
    if (length(mapped_full_row) == 0 || is.na(mapped_full_row)) {
      selected_table(tibble::tibble(
        gene_id = character(),
        gene_name = character(),
        exp_value = numeric(),
        description = character()
      ))

      showModal(modalDialog(
        title = paste("Selected cell ID:", as.character(cell_key)),
        div(class = "width-100", DT::DTOutput("cell_expression_table")),
        size = "xl",
        easyClose = TRUE,
        footer = modalButton(label = tagList(icon("times")))
      ))
      return()
    }

    if (!is.data.frame(full_mat)) full_mat <- as.data.frame(full_mat)

    # Build a one-row data.frame with an 'id' column for pivoting
    expr_row <- full_mat[mapped_full_row, gene_cols, drop = FALSE]
    expr_row$id <- mapped_full_row

    if (length(gene_cols) == 0) {
      long_tbl <- tibble::tibble(
        gene_id = character(),
        gene_name = character(),
        exp_value = numeric(),
        description = character()
      )
    } else {
      long_tbl <- expr_row %>%
        pivot_longer(
          cols = -id,
          names_to = "gene_id",
          values_to = "expression"
        ) %>%
        filter(expression > 0) %>%
        left_join(gff_data, by = c("gene_id" = "id")) %>%
        arrange(desc(expression)) %>%
        rename(
          cell_id = id,
          gene_name = name,
          exp_value = expression
        ) %>%
        select(gene_id, gene_name, exp_value, description)
    }

    selected_table(long_tbl)

    # Showing the actual barcode
    sel_barcode <- tryCatch(
      {
        if (!is.null(mapped_full_row) && !is.na(mapped_full_row) && "barcode" %in% colnames(full_mat)) {
          as.character(full_mat$barcode[mapped_full_row])
        } else {
          as.character(cell_key)
        }
      },
      error = function(e) as.character(cell_key)
    )

    showModal(modalDialog(
      title = paste("Selected cell ID:", sel_barcode),
      div(class = "width-100", DT::DTOutput("cell_expression_table")),
      size = "xl",
      easyClose = TRUE,
      footer = tagList(
        downloadButton("export_data", "Export .csv", class = "btn-success"),
        modalButton(label = tagList(icon("times")))
      )
    ))
  })

  # render the cell_expression_table
  output$cell_expression_table <- DT::renderDT({
    req(selected_table())
    datatable(
      selected_table(),
      options = list(
        pageLength = 15,
        scrollY = "400px",
        scrollCollapse = TRUE,
        paging = TRUE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      filter = "top",
      selection = "single",
      width = "100%"
    ) %>%
      formatRound("exp_value", digits = 3)
  })

  # Clear selected table and cell cache when modal is dismissed
  observeEvent(input$shiny_modal, {
    if (!isTruthy(input$shiny_modal)) {
      selected_table(NULL)
      cache_keys <- names(cell_data_cache)
      cell_keys <- cache_keys[grepl("^cell_", cache_keys)]
      if (length(cell_keys) > 10) {
        old_keys <- head(cell_keys, length(cell_keys) - 10)
        for (key in old_keys) {
          cell_data_cache[[key]] <- NULL
        }
      }
    }
  })

  ## handle export data
  output$export_data <- downloadHandler(
    filename = function() {
      paste("cell_expression_data", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.4, detail = "Saving file...")
        write.csv(selected_table(), file, row.names = FALSE)
        incProgress(1, detail = "Download complete")
      })
    }
  )

  # handle download data
  output$download_data <- downloadHandler(
    filename = function() {
      base_name <- "Plasmodium_vivax_embedding_data"

      # Add gene information to filename if coloring by genes
      if (!is.null(input$color) && input$color == "genes" && !is.null(input$genes_n) && length(input$genes_n) > 0) {
        if (length(input$genes_n) == 1) {
          base_name <- paste0(base_name, "_", input$genes_n[1])
        } else {
          base_name <- paste0(base_name, "_", length(input$genes_n), "_genes")
        }
      }

      paste0(base_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.4, detail = "Saving file...")
        write.csv(filtered_data(), file, row.names = FALSE)
        incProgress(1, detail = "Download complete")
      })
    }
  )

  # Handle download for first gene expression plot (plot 0)
  output$download_gene_exp_0 <- downloadHandler(
    filename = function() {
      gene_names <- tryCatch(input$genes_exp_0, error = function(e) "gene")
      if (length(gene_names) > 1) {
        paste0("gene_expression_", length(gene_names), "_genes_", Sys.Date(), ".xlsx")
      } else {
        paste0("gene_expression_", gene_names, "_", Sys.Date(), ".xlsx")
      }
    },
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.3, detail = "Preparing data...")

        tryCatch(
          {
            fg <- filtered_gene_exp_data()
            cur_genes <- isolate(input$genes_exp_0)
            cur_group <- isolate(input$group_exp_0)
            cur_check <- isolate(input$check_exp_0)

            if (!is.null(cur_genes) && !is.null(cur_group) && length(cur_genes) > 0) {
              # Handle multiple gene selection
              if (isTRUE(cur_check)) {
                gene_exp_val <- cur_genes
              } else {
                gene_exp_val <- sapply(cur_genes, function(gene) get_gene_id(gene, fg$genes), USE.NAMES = FALSE)
              }

              # Remove any invalid gene IDs
              valid_genes <- cur_genes[!is.na(gene_exp_val) & gene_exp_val != ""]
              valid_gene_ids <- gene_exp_val[!is.na(gene_exp_val) & gene_exp_val != ""]

              if (length(valid_gene_ids) == 0) {
                stop("No valid genes found for the current selection")
              }

              plot_data <- build_plot_slim(fg$data, fg$full, valid_gene_ids, cur_group)

              if (!is.null(plot_data) && nrow(plot_data) > 0) {
                incProgress(0.5, detail = "Creating metadata...")

                # Create metadata sheet with analysis parameters
                gene_info <- if (length(valid_gene_ids) == 1) {
                  list(
                    gene_id = as.character(valid_gene_ids[1]),
                    gene_name = as.character(valid_genes[1]),
                    analysis_type = "Single Gene"
                  )
                } else {
                  list(
                    gene_id = paste(valid_gene_ids, collapse = ", "),
                    gene_name = paste(valid_genes, collapse = ", "),
                    analysis_type = paste("Average of", length(valid_gene_ids), "genes")
                  )
                }

                metadata <- data.frame(
                  Parameter = c(
                    "Analysis Type",
                    "Data Type",
                    "Gene ID(s)",
                    "Gene Name(s)",
                    "Group By",
                    "Study Label",
                    "Host Species",
                    "Parasite stages",
                    "Treatment",
                    "Development phase",
                    "Sample Type",
                    "Strain",
                    "Hour Post Invasion",
                    "Sequencer",
                    "SC Technology",
                    "Download Date"
                  ),
                  Value = c(
                    gene_info$analysis_type,
                    isolate(input$data_type_exp),
                    gene_info$gene_id,
                    gene_info$gene_name,
                    as.character(cur_group),
                    paste(isolate(input$study_label), collapse = ", "),
                    paste(isolate(input$host_species), collapse = ", "),
                    paste(isolate(input$parasite_stages), collapse = ", "),
                    paste(isolate(input$treatment), collapse = ", "),
                    paste(isolate(input$development_phase), collapse = ", "),
                    paste(isolate(input$sample_type), collapse = ", "),
                    paste(isolate(input$strain), collapse = ", "),
                    paste(isolate(input$hour_post_invasion), collapse = ", "),
                    paste(isolate(input$sequencer), collapse = ", "),
                    paste(isolate(input$sc_technology), collapse = ", "),
                    as.character(Sys.time())
                  ),
                  stringsAsFactors = FALSE
                )

                incProgress(0.7, detail = "Saving file...")

                # Create Excel file with two sheets
                writexl::write_xlsx(
                  list(
                    "Expression Data" = plot_data,
                    "Metadata" = metadata
                  ),
                  path = file
                )

                incProgress(1, detail = "Download complete")
              } else {
                stop("No data available for the selected gene and filters")
              }
            } else {
              stop("Please select a gene and grouping option")
            }
          },
          error = function(e) {
            # Create a simple error Excel file instead of failing completely
            error_data <- data.frame(
              Error = "Download failed",
              Message = e$message,
              Suggestion = "Please try again or contact support",
              stringsAsFactors = FALSE
            )
            writexl::write_xlsx(list("Error" = error_data), path = file)
          }
        )
      })
    }
  )

  # Create download handlers dynamically for additional gene expression plots (1+)
  observe({
    for (i in 1:20) {
      local({
        plot_id <- i
        output_name <- paste0("download_gene_exp_", plot_id)

        output[[output_name]] <- downloadHandler(
          filename = function() {
            gene_names <- tryCatch(
              {
                gn <- input[[paste0("genes_exp_", plot_id)]]
                if (is.null(gn) || length(gn) == 0) "gene" else gn
              },
              error = function(e) "gene"
            )

            if (length(gene_names) > 1) {
              paste0("gene_expression_", length(gene_names), "_genes_", Sys.Date(), ".xlsx")
            } else {
              paste0("gene_expression_", gene_names[1], "_", Sys.Date(), ".xlsx")
            }
          },
          content = function(file) {
            withProgress(message = "Processing Data", value = 0, {
              incProgress(0.3, detail = "Preparing data...")

              tryCatch(
                {
                  # Validate all required inputs exist
                  cur_genes <- input[[paste0("genes_exp_", plot_id)]]
                  cur_group <- input[[paste0("group_exp_", plot_id)]]
                  cur_check <- input[[paste0("check_exp_", plot_id)]]

                  if (is.null(cur_genes) || is.null(cur_group) || length(cur_genes) == 0) {
                    showNotification("Please select a gene and grouping option first", type = "error")
                    return(NULL)
                  }

                  fg <- filtered_gene_exp_data()
                  if (is.null(fg) || is.null(fg$genes)) {
                    showNotification("No data available for download", type = "error")
                    return(NULL)
                  }

                  # Handle multiple gene selection
                  if (isTRUE(cur_check)) {
                    gene_exp_val <- cur_genes
                  } else {
                    gene_exp_val <- sapply(cur_genes, function(gene) get_gene_id(gene, fg$genes), USE.NAMES = FALSE)
                  }

                  # Remove any invalid gene IDs
                  valid_genes <- cur_genes[!is.na(gene_exp_val) & gene_exp_val != ""]
                  valid_gene_ids <- gene_exp_val[!is.na(gene_exp_val) & gene_exp_val != ""]

                  if (length(valid_gene_ids) == 0) {
                    showNotification("No valid genes found for the current selection", type = "error")
                    return(NULL)
                  }

                  plot_data <- build_plot_slim(fg$data, fg$full, valid_gene_ids, cur_group)

                  if (!is.null(plot_data) && nrow(plot_data) > 0) {
                    incProgress(0.5, detail = "Creating metadata...")

                    # Create gene info for metadata
                    gene_info <- if (length(valid_gene_ids) == 1) {
                      list(
                        gene_id = as.character(valid_gene_ids[1]),
                        gene_name = as.character(valid_genes[1]),
                        analysis_type = "Single Gene"
                      )
                    } else {
                      list(
                        gene_id = paste(valid_gene_ids, collapse = ", "),
                        gene_name = paste(valid_genes, collapse = ", "),
                        analysis_type = paste("Average of", length(valid_gene_ids), "genes")
                      )
                    }

                    # Create metadata sheet with analysis parameters
                    metadata <- data.frame(
                      Parameter = c(
                        "Analysis Type",
                        "Data Type",
                        "Gene ID(s)",
                        "Gene Name(s)",
                        "Group By",
                        "Study Label",
                        "Host Species",
                        "Parasite stages",
                        "Treatment",
                        "Development phase",
                        "Sample Type",
                        "Strain",
                        "Hour Post Invasion",
                        "Sequencer",
                        "SC Technology",
                        "Download Date"
                      ),
                      Value = c(
                        gene_info$analysis_type,
                        isolate(input$data_type_exp),
                        gene_info$gene_id,
                        gene_info$gene_name,
                        as.character(cur_group),
                        paste(isolate(input$study_label), collapse = ", "),
                        paste(isolate(input$host_species), collapse = ", "),
                        paste(isolate(input$parasite_stages), collapse = ", "),
                        paste(isolate(input$treatment), collapse = ", "),
                        paste(isolate(input$development_phase), collapse = ", "),
                        paste(isolate(input$sample_type), collapse = ", "),
                        paste(isolate(input$strain), collapse = ", "),
                        paste(isolate(input$hour_post_invasion), collapse = ", "),
                        paste(isolate(input$sequencer), collapse = ", "),
                        paste(isolate(input$sc_technology), collapse = ", "),
                        as.character(Sys.time())
                      ),
                      stringsAsFactors = FALSE
                    )

                    incProgress(0.7, detail = "Saving file...")

                    # Create Excel file with two sheets
                    writexl::write_xlsx(
                      list(
                        "Expression Data" = plot_data,
                        "Metadata" = metadata
                      ),
                      path = file
                    )

                    incProgress(1, detail = "Download complete")
                  } else {
                    showNotification("No data to download", type = "warning")
                  }
                },
                error = function(e) {
                  showNotification(paste("Error downloading:", e$message), type = "error")
                }
              )
            })
          }
        )
      })
    }
  })

  # Simple memory cleanup on session end
  onSessionEnded(function() {
    tryCatch(
      {
        # Clear memoise cache
        if (exists("load_data") && is.function(load_data)) {
          memoise::forget(load_data)
        }

        # Run garbage collection
        gc()
      },
      error = function(e) {}
    )
  })
}
