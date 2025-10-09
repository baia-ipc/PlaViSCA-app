library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(later)

server <- function(input, output, session) {
  sessionData <- new.env(parent = emptyenv())

  # Reactive expression for publication data - updates when CSV file changes
  pub_data <- reactive({
    readr::read_csv("data/data_source.csv", show_col_types = FALSE)
  })

  autoUpdatePlot <- reactiveVal(FALSE)

  # Global loading state for better UX
  global_loading <- reactiveVal(FALSE)

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

  # Larger screen adjustment
  observe({
    if (
      input$tabset == "Metadata" ||
        input$tabset == "Quality control" ||
        input$tabset == "Data sources" ||
        input$tabset == "Gene expression"
    ) {
      shinyjs::removeClass("sidebar", "visible")
      shinyjs::runjs('$("#main").addClass("col-sm-12");')
    } else {
      shinyjs::addClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-12");')
    }
  })

  # Synchronize data type selection across tabs
  observeEvent(input$tabset, {
    syncing_tabs(TRUE) # Set flag to prevent reactive loops

    if (input$tabset == "Gene expression") {
      # Sync with embeddings data type if available, otherwise default to normalize
      current_selection <- if (!is.null(input$data_type)) {
        input$data_type
      } else {
        "normalize"
      }

      # Only update if the selection is different to avoid unnecessary triggers
      if (is.null(input$data_type_exp) || input$data_type_exp != current_selection) {
        updateRadioButtons(
          session,
          "data_type_exp",
          choices = list(
            "Raw" = "raw",
            "Normalized" = "normalize"
            # "Scaled" = "scale"
          ),
          selected = current_selection
        )
      }
    } else if (input$tabset == "Embeddings") {
      current_selection <- if (!is.null(input$data_type_exp)) {
        input$data_type_exp
      } else {
        "normalize"
      }

      # Only update if the selection is different to avoid unnecessary triggers
      if (is.null(input$data_type) || input$data_type != current_selection) {
        updateRadioButtons(
          session,
          "data_type",
          choices = list(
            "Raw" = "raw",
            "Normalized" = "normalize"
            # "Scaled" = "scale"
          ),
          selected = current_selection
        )
      }
    }

    # Reset flag after a short delay
    later(function() {
      syncing_tabs(FALSE)
    }, 0.1)
  })

  # Data caching system for performance optimization
  default_df_filter <- reactiveVal(NULL)
  app_initialized <- reactiveVal(FALSE)
  data_loading_trigger <- reactiveVal(0)
  syncing_tabs <- reactiveVal(FALSE) # Flag to prevent sync loops

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
    if (is.null(cached_data[[data_type]])) {
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
    } else {
      return(cached_data[[data_type]])
    }
  }

  # Debounced observer to prevent multiple rapid triggers - ONLY for data type changes
  observe({
    list(
      input$data_type,
      input$data_type_exp
    )

    # Skip if we're in the middle of tab synchronization
    if (syncing_tabs()) {
      return()
    }

    # Only trigger after app is properly initialized
    if (!is.null(input$data_type) || !is.null(input$data_type_exp)) {
      if (!app_initialized()) {
        app_initialized(TRUE)
      }
    }

    # Use later() to debounce - only trigger after 500ms of no changes
    if (app_initialized()) {
      later(
        function() {
          isolate({
            data_loading_trigger(data_loading_trigger() + 1)
          })
        },
        0.5
      )
    }
  })

  # Main gene_data reactive - ONLY loads data, no filtering
  gene_data <- eventReactive(
    data_loading_trigger(),
    {
      if (!app_initialized()) {
        return(NULL)
      }

      current_type <- get_current_data_type()
      req(current_type)

      # Check if we can reuse cached data based ONLY on data type
      if (
        !is.null(cached_data$current_type) &&
          cached_data$current_type == current_type
      ) {
        return(sessionData$last_gene_data)
      }

      # Show progress bar when loading new data
      withProgress(message = "", value = 0, {
        display_name <- switch(current_type,
          raw = "Raw",
          normalize = "Normalized",
          # scale = "Scaled",
        )

        # Check if dataset is already cached
        if (!is.null(cached_data[[current_type]])) {
          setProgress(
            0.3,
            detail = paste("Using cached", display_name, "dataset...")
          )
        } else {
          setProgress(0.3, detail = paste("Loading", display_name, "data..."))
        }

        # Load dataset (from cache if available)
        dataset <- load_dataset(current_type)

        if (is.null(dataset)) {
          showNotification("Error: Dataset not available", type = "error")
          return(NULL)
        }

        setProgress(0.6, detail = "Processing genes...")
        result <- list(
          data = dataset,
          genes = genes(dataset, gff_data)
        )

        # Cache the current state (only data type, no filter)
        cached_data$current_type <- current_type
        sessionData$last_gene_data <- result

        setProgress(1, detail = "Complete!")
        message("Loaded and cached data for type: ", current_type)

        return(result)
      })
    },
    ignoreNULL = FALSE
  )

  # Separate reactive for filtered gene data - no progress bar
  filtered_gene_data <- reactive({
    req(gene_data())

    selections <- input$apply_filter

    # If no selections made (empty or null), use default filter
    if (is.null(selections) || length(selections) == 0) {
      selections <- default_df_filter()
    }

    req(selections)

    # Create cache key for filtered gene data
    cache_key <- make_cache_key("filtered_gene_data", get_current_data_type(), selections)

    # Check cache first to avoid reprocessing
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }

    # Filter the data without progress bar
    result <- list(
      data = fun_filter_df(gene_data()$data, selections),
      genes = gene_data()$genes
    )

    # Cache the result
    sessionData[[cache_key]] <- result

    return(result)
  })

  # Synchronize data type selections between tabs using helper functions
  create_data_type_sync_observer(
    input = input,
    session = session,
    input_name = "data_type",
    target_input = "data_type_exp",
    expected_tabset = "Embeddings",
    global_loading = global_loading
  )

  create_data_type_sync_observer(
    input = input,
    session = session,
    input_name = "data_type_exp",
    target_input = "data_type",
    expected_tabset = "Gene expression",
    global_loading = global_loading
  )



  # Provide loading feedback to UI
  output$loading_state <- reactive({
    global_loading()
  })
  outputOptions(output, "loading_state", suspendWhenHidden = FALSE)

  # Metadata filter
  # choose columns for filter data
  grouped_choices <- list(
    parasite_stage = setNames(
      paste0("parasite_stage__", unique(mr_data$parasite_stage)),
      unique(mr_data$parasite_stage)
    ),
    study_label = setNames(
      paste0("study_label__", unique(mr_data$study_label)),
      unique(mr_data$study_label)
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

  user_interact <- reactiveVal(FALSE)
  observeEvent(input$apply_filter, {
    user_interact(TRUE)
  })

  # Reactive filtered data with memory management and caching
  filtered_data <- reactive({
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

    # Process data
    result <- fun_filter_df(mr_data, selections)

    # Cache the result
    sessionData[[cache_key]] <- result

    return(result)
  })

  # Manipulation tabset
  observe({
    filt_data <- filtered_data()
    req(nrow(filt_data) > 0)

    if (input$tabset == "Metadata") {
      updateSelectInput(
        session,
        "study_label",
        choices = unique(filt_data$study_label)
      )
      updateSelectInput(
        session,
        "sample_type",
        choices = unique(filt_data$sample_type)
      )
      updateSelectInput(
        session,
        "host_species",
        choices = setNames(
          unique(filt_data$host_species),
          format_species_name(unique(filt_data$host_species))
        )
      )
      updateSelectInput(
        session,
        "life_cycle_stage",
        choices = unique(filt_data$life_cycle_stage)
      )
      updateSelectInput(
        session,
        "treatment",
        choices = unique(filt_data$treatment)
      )
      updateSelectInput(
        session,
        "parasite_stage",
        choices = unique(filt_data$parasite_stage)
      )
      updateSelectInput(
        session,
        "strain",
        choices = unique(filt_data$strain)
      )
      updateSelectInput(
        session,
        "hour_post_invasion",
        choices = unique(filt_data$hour_post_invasion)
      )
      updateSelectInput(
        session,
        "sequencer",
        choices = unique(filt_data$sequencer)
      )
      updateSelectInput(
        session,
        "sc_technology",
        choices = unique(filt_data$sc_technology)
      )
    } else if (input$tabset == "Quality control") {
      updateSelectInput(
        session,
        "study_label_qc",
        choices = unique(filt_data$study_label)
      )
      updateSelectInput(
        session,
        "sample_type_qc",
        choices = unique(filt_data$sample_type)
      )
      updateSelectInput(
        session,
        "host_species_qc",
        choices = setNames(
          unique(filt_data$host_species),
          format_species_name(unique(filt_data$host_species))
        )
      )
      updateSelectInput(
        session,
        "life_cycle_stage_qc",
        choices = unique(filt_data$life_cycle_stage)
      )
      updateSelectInput(
        session,
        "treatment_qc",
        choices = unique(filt_data$treatment)
      )
      updateSelectInput(
        session,
        "parasite_stage_qc",
        choices = unique(filt_data$parasite_stage)
      )
      updateSelectInput(
        session,
        "strain_qc",
        choices = unique(filt_data$strain)
      )
      updateSelectInput(
        session,
        "hour_post_invasion_qc",
        choices = unique(filt_data$hour_post_invasion)
      )
      updateSelectInput(
        session,
        "sequencer_qc",
        choices = unique(filt_data$sequencer)
      )
      updateSelectInput(
        session,
        "sc_technology_qc",
        choices = unique(filt_data$sc_technology)
      )
    } else if (input$tabset == "Gene expression") {
      updateSelectInput(
        session,
        "study_label_exp",
        choices = unique(filt_data$study_label)
      )
      updateSelectInput(
        session,
        "sample_type_exp",
        choices = unique(filt_data$sample_type)
      )
      updateSelectInput(
        session,
        "host_species_exp",
        choices = setNames(
          unique(filt_data$host_species),
          format_species_name(unique(filt_data$host_species))
        )
      )
      updateSelectInput(
        session,
        "life_cycle_stage_exp",
        choices = unique(filt_data$life_cycle_stage)
      )
      updateSelectInput(
        session,
        "treatment_exp",
        choices = unique(filt_data$treatment)
      )
      updateSelectInput(
        session,
        "parasite_stage_exp",
        choices = unique(filt_data$parasite_stage)
      )
      updateSelectInput(
        session,
        "strain_exp",
        choices = unique(filt_data$strain)
      )
      updateSelectInput(
        session,
        "hour_post_invasion_exp",
        choices = unique(filt_data$hour_post_invasion)
      )
      updateSelectInput(
        session,
        "sequencer_exp",
        choices = unique(filt_data$sequencer)
      )
      updateSelectInput(
        session,
        "sc_technology_exp",
        choices = unique(filt_data$sc_technology)
      )
    }
  })

  observeEvent(
    list(
      input$data_opt,
      input$study_label,
      input$host_species,
      input$life_cycle_stage,
      input$treatment,
      input$parasite_stage,
      input$sample_type,
      input$strain,
      input$hour_post_invasion,
      input$sequencer,
      input$sc_technology
    ),
    {
      current_inputs <- list(
        study_label = input$study_label,
        sample_type = input$sample_type,
        host_species = input$host_species,
        life_cycle_stage = input$life_cycle_stage,
        treatment = input$treatment,
        parasite_stage = input$parasite_stage,
        strain = input$strain,
        hour_post_invasion = input$hour_post_invasion,
        sequencer = input$sequencer,
        sc_technology = input$sc_technology
      )

      filt_data <- apply_secondary_filters(filtered_data(), current_inputs)

      output$data_table <- renderDT(
        {
          # Create metadata table with filtered columns and proper formatting
          processed_data <- filt_data %>%
            select(-c(
              biological_replicate,
              liver_form,
              starts_with("rna_snn_res_"),
              starts_with("umap_"),
              starts_with("pca_"),
              starts_with("t_sne_"),
              barcode,
              study_pmid,
              run_id,
              num_srr,
              pub_year,
              host_id,
              goegraphic_location,
              sequncer,
              refine_state,
              pred_gametocyte,
              seurat_clusters,
              parasite_stage,
              geographic_location,
              day_post_infection
            )) %>%
            rename_with(
              ~ str_to_title(.),
              .cols = any_of(c(
                "study_label",
                "sample_type",
                "host_species",
                "life_cycle_stage",
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
              Life_cycle_stage,
              Hour_post_invasion,
              Sc_technology,
              Sequencer,
              nCount_RNA,
              nFeature_RNA
            )

          DT::datatable(
            processed_data,
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

      ## plot life_cycle vs count
      output$life_cycle_plotly <- renderPlotly({
        meta_plotly(
          filt_data,
          "life_cycle_stage",
          "Count",
          "Life cyle stage",
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
      input$life_cycle_stage_qc,
      input$treatment_qc,
      input$parasite_stage_qc,
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
        life_cycle_stage = input$life_cycle_stage_qc,
        treatment = input$treatment_qc,
        parasite_stage = input$parasite_stage_qc,
        sample_type = input$sample_type_qc,
        strain = input$strain_qc,
        hour_post_invasion = input$hour_post_invasion_qc,
        sequencer = input$sequencer_qc,
        sc_technology = input$sc_technology_qc
      )

      filt_data <- apply_secondary_filters(filtered_data(), current_inputs)

      ## plot nCount in QC tab
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

      ## plot nFeature in QC tab
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

      ## plot elbow
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

      ## plot variable genes
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
  )

  # Gene Expression tabset
  # Simplified filtered gene expression data for speed
  filtered_gene_exp_data <- reactive({
    req(filtered_gene_data())

    current_inputs <- list(
      study_label = input$study_label_exp,
      host_species = input$host_species_exp,
      life_cycle_stage = input$life_cycle_stage_exp,
      treatment = input$treatment_exp,
      parasite_stage = input$parasite_stage_exp,
      sample_type = input$sample_type_exp,
      strain = input$strain_exp,
      hour_post_invasion = input$hour_post_invasion_exp,
      sequencer = input$sequencer_exp,
      sc_technology = input$sc_technology_exp
    )

    # Create cache key
    cache_key <- make_cache_key(
      "filtered_exp",
      get_current_data_type(),
      input$study_label_exp,
      input$host_species_exp,
      input$life_cycle_stage_exp,
      input$treatment_exp,
      input$parasite_stage_exp,
      input$sample_type_exp,
      input$strain_exp,
      input$hour_post_invasion_exp,
      input$sequencer_exp,
      input$sc_technology_exp
    )

    # Check cache first - use exists()
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }
    tryCatch(
      {
        filt_data <- apply_secondary_filters(filtered_gene_data()$data, current_inputs)

        result <- list(
          data = filt_data,
          genes = filtered_gene_data()$genes
        )

        # Cache the result
        sessionData[[cache_key]] <- result

        return(result)
      },
      error = function(e) {
        return(NULL)
      }
    )
  })

  observeEvent(
    list(
      input$data_opt,
      input$study_label_exp,
      input$host_species_exp,
      input$life_cycle_stage_exp,
      input$treatment_exp,
      input$parasite_stage_exp,
      input$sample_type_exp,
      input$strain_exp,
      input$hour_post_invasion_exp,
      input$sequencer_exp,
      input$sc_technology_exp
    ),
    {
      current_inputs <- list(
        study_label = input$study_label_exp,
        host_species = input$host_species_exp,
        life_cycle_stage = input$life_cycle_stage_exp,
        treatment = input$treatment_exp,
        parasite_stage = input$parasite_stage_exp,
        sample_type = input$sample_type_exp,
        strain = input$strain_exp,
        hour_post_invasion = input$hour_post_invasion_exp,
        sequencer = input$sequencer_exp,
        sc_technology = input$sc_technology_exp
      )

      filt_data <- apply_secondary_filters(filtered_data(), current_inputs)
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
              list(width = "500px", targets = 3, className = "dt-left")
            )
          ),
          selection = "single",
          rownames = FALSE
        )
      })
    }
  })

  # Manipulation tabset "Gene Exp"
  ## gene_exp_0 block with minimal caching for speed
  choice_exp_0 <- reactive({
    req(gene_data()$genes)
    get_gene_choices(gene_data()$genes, input$check_exp_0)
  })

  # Use observer for gene choice updates
  observe({
    req(choice_exp_0())
    tryCatch(
      {
        updateSelectizeInput(
          session,
          "genes_exp_0",
          label = if (isTRUE(input$check_exp_0)) "Gene ID" else "Gene name",
          choices = choice_exp_0(),
          server = TRUE
        )
      },
      error = function(e) {
        # Silent error handling
      }
    )
  })

  # df_exp data for genes_exp_0 with filter-aware caching
  df_exp_0 <- reactive({
    req(filtered_gene_exp_data(), input$genes_exp_0, input$group_exp)

    # Skip processing during loading states
    if (global_loading()) {
      return(NULL)
    }

    # Create a safe cache key without digest
    cache_key <- make_cache_key(
      "df_exp",
      input$genes_exp_0,
      input$group_exp,
      input$check_exp_0,
      get_current_data_type(),
      input$study_label_exp,
      input$host_species_exp,
      input$life_cycle_stage_exp,
      input$treatment_exp,
      input$parasite_stage_exp,
      input$sample_type_exp,
      input$strain_exp,
      input$hour_post_invasion_exp,
      input$sequencer_exp,
      input$sc_technology_exp
    )

    # Check cache first for instant gene switching - use exists() for safety
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }

    # Compute result efficiently
    result <- tryCatch(
      {
        df_exp0(
          gene_data = filtered_gene_exp_data(),
          gene_input = input$genes_exp_0,
          group_input = input$group_exp,
          check_exp = input$check_exp_0
        )
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Cache only successful results
    if (!is.null(result) && nrow(result) > 0) {
      sessionData[[cache_key]] <- result
    }

    return(result)
  })

  # Simplified gene expression plot for maximum speed
  output$gene_exp_plotly <- renderPlotly({
    req(df_exp_0(), input$group_exp)

    if (global_loading()) {
      return(NULL)
    }

    # Create cache key
    plot_cache_key <- make_cache_key(
      "plot",
      input$genes_exp_0,
      input$group_exp,
      get_current_data_type(),
      input$study_label_exp,
      input$host_species_exp,
      input$life_cycle_stage_exp,
      input$treatment_exp,
      input$parasite_stage_exp,
      input$sample_type_exp,
      input$strain_exp,
      input$hour_post_invasion_exp,
      input$sequencer_exp,
      input$sc_technology_exp
    )

    # Check cache for instant gene switching - use exists() for safety
    if (exists(plot_cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[plot_cache_key]])
    }

    # Generate plot efficiently
    result <- tryCatch(
      {
        group <- tolower(input$group_exp)
        gene_exp_plot(df_exp_0(), group)
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Cache successful plots
    if (!is.null(result)) {
      sessionData[[plot_cache_key]] <- result
    }

    return(result)
  })

  ## gene expression link
  observeEvent(input$genes_exp_0, {
    req(input$genes_exp_0)
    gene_id <- get_gene_id(input$genes_exp_0, gene_data()$genes)
    gene_links(gene_id)
  })

  ## gene_exp_i block
  ## observe add button click from js
  choice_exp <- reactiveVal()
  counter <- reactiveVal(0)
  # observeEvent(input$click_add_btn, {
  #   btn_id <- input$click_add_btn
  #   id <- as.numeric(gsub("add_btn_?", "", btn_id))
  #   shinyjs::hide(paste0("add_btn_", id))
  #
  #   choice_exp(get_gene_choices(gene_data()$genes, FALSE))
  #
  #   ## add ui block to the right after gene_exp_tar
  #   if (id == 0) {
  #     id <- id + 1
  #     insertUI(
  #       selector = "#gene_exp_tar",
  #       where = "afterEnd",
  #       ui = column(
  #         width = 6,
  #         id = paste0("dynamic_col_", id),
  #         subBlock(id)
  #       )
  #     )
  #     shinyjs::delay(100, {
  #       updateSelectizeInput(
  #         session,
  #         inputId = paste0("genes_exp_", id),
  #         choices = choice_exp(),
  #         server = TRUE
  #       )
  #     })
  #
  #     # Create a properly scoped reactive for this specific plot with shorter delay
  #     shinyjs::delay(100, {
  #       output[[paste0("gene_exp_plotly_", id)]] <- local({
  #         plot_id <- id
  #         renderPlotly({
  #           # Only proceed if we have data and inputs
  #           req(
  #             gene_data(),
  #             input[[paste0("genes_exp_", plot_id)]],
  #             input[[paste0("group_exp_", plot_id)]]
  #           )
  #
  #           # Skip during loading states
  #           if (global_loading()) {
  #             return(NULL)
  #           }
  #
  #           tryCatch(
  #             {
  #               # Create the reactive data for this specific plot
  #               df_reactive <- df_expi(
  #                 id = plot_id,
  #                 input = input,
  #                 gene_data = gene_data(),
  #                 get_gene_id = get_gene_id
  #               )
  #
  #               req(df_reactive())
  #
  #               gene_exp_plot(
  #                 df_reactive(),
  #                 tolower(input[[paste0("group_exp_", plot_id)]])
  #               )
  #             },
  #             error = function(e) {
  #               message(
  #                 "Error in dynamic gene expression plot ",
  #                 plot_id,
  #                 ": ",
  #                 e$message
  #               )
  #               return(NULL)
  #             }
  #           )
  #         })
  #       })
  #     })
  #   } else {
  #     ## add ui block to button
  #     id <- id + 1
  #     newBlock(id, counter, choice_exp(), subBlock, session)
  #
  #     # Create a properly scoped reactive for this specific plot with shorter delay
  #     shinyjs::delay(100, {
  #       output[[paste0("gene_exp_plotly_", id)]] <- local({
  #         plot_id <- id
  #         renderPlotly({
  #           # Only proceed if we have data and inputs
  #           req(
  #             gene_data(),
  #             input[[paste0("genes_exp_", plot_id)]],
  #             input[[paste0("group_exp_", plot_id)]]
  #           )
  #
  #           # Skip during loading states
  #           if (global_loading()) {
  #             return(NULL)
  #           }
  #
  #           tryCatch(
  #             {
  #               # Create the reactive data for this specific plot
  #               df_reactive <- df_expi(
  #                 id = plot_id,
  #                 input = input,
  #                 gene_data = gene_data(),
  #                 get_gene_id = get_gene_id
  #               )
  #
  #               req(df_reactive())
  #
  #               gene_exp_plot(
  #                 df_reactive(),
  #                 tolower(input[[paste0("group_exp_", plot_id)]])
  #               )
  #             },
  #             error = function(e) {
  #               message(
  #                 "Error in dynamic gene expression plot ",
  #                 plot_id,
  #                 ": ",
  #                 e$message
  #               )
  #               return(NULL)
  #             }
  #           )
  #         })
  #       })
  #     })
  #   }
  # })

  ## observe remove button click from js
  # observeEvent(
  #   input$click_remove_btn,
  #   {
  #     rm_btn_id <- input$click_remove_btn$btn_id
  #     id <- as.numeric(gsub("remove_btn_", "", rm_btn_id))
  #     ids <- as.numeric(unlist(input$click_remove_btn$ids))
  #     removeUI(selector = paste0("#dynamic_col_", id))
  #     remaining_ids <- ids[ids != id]
  #
  #     for (i in remaining_ids) {
  #       shinyjs::hide(paste0("add_btn_", i))
  #     }
  #
  #     if (length(remaining_ids) == 0) {
  #       shinyjs::show("add_btn_0")
  #     } else {
  #       shinyjs::show(paste0("add_btn_", max(remaining_ids)))
  #     }
  #   },
  #   ignoreNULL = TRUE
  # )

  ## update gene_id or name in SelectizeInput when toggle button click
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
    gene_id <- get_gene_id(input[[paste0("genes_exp_", id)]], gene_data()$genes)
    gene_links(gene_id)
  })

  ## update plot when data_type_exp_change from js with debouncing
  gene_exp_plots_trigger <- reactiveVal(0)

  # Store dynamic plot reactive functions for proper invalidation
  dynamic_plot_reactives <- reactiveValues()

  observeEvent(input$data_type_exp_change, {
    req(gene_data(), input$data_type_exp_change)

    # Debounce multiple plot updates with shorter delay
    later(
      function() {
        isolate({
          gene_exp_plots_trigger(gene_exp_plots_trigger() + 1)
        })
      },
      0.2
    )
  })

  # Manipulation tabset "Embedding"
  observeEvent(input$genes_n, {
    req(input$genes_n)
    if (input$color == "genes") {
      gene_id <- get_gene_id(input$genes_n, gene_data()$genes)
      gene_links(gene_id)
    }
  })

  choice <- eventReactive(list(input$check, input$data_type), {
    req(gene_data()$genes) # Ensure gene data is available
    get_gene_choices(gene_data()$genes, input$check)
  })

  ## dynamic UI for gene selection (RNA_snn commented out)
  shinyjs::hide("gene_snn_ui")
  shinyjs::hide("data_type")
  observe({
    if (input$color == "genes") {
      shinyjs::show("data_type")
      shinyjs::show("gene_snn_ui")
      shinyjs::show("gene_link_ui")
      shinyjs::show("switch_id")

      # Use a safer approach to get gene choices
      tryCatch(
        {
          gene_choices <- if (!is.null(gene_data()$genes)) {
            get_gene_choices(gene_data()$genes, input$check)
          } else {
            ""
          }

          updateSelectizeInput(
            session,
            "genes_n",
            label = if (isTRUE(input$check)) "Gene ID" else "Gene name",
            choices = gene_choices,
            server = TRUE
          )
        },
        error = function(e) {
          # Fallback to empty choices if there's an error
          updateSelectizeInput(
            session,
            "genes_n",
            label = if (isTRUE(input$check)) "Gene ID" else "Gene name",
            choices = "",
            server = TRUE
          )
        }
      )
      # } else if (input$color == "RNA_snn") {
      #   shinyjs::show("gene_snn_ui")
      #   shinyjs::hide("gene_link_ui")
      #   shinyjs::hide("data_type")
      #   shinyjs::hide("switch_id")
      #   updateSelectizeInput(
      #     session,
      #     "genes_n",
      #     label = "Select Resolution",
      #     choices = RNA_snn,
      #     server = TRUE
      #   )
    } else {
      shinyjs::hide("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
      shinyjs::hide("data_type")
      shinyjs::hide("switch_id")
    }
  })

  # Separate observer to update gene choices when data or toggle changes
  observe({
    req(gene_data()$genes, input$color == "genes")

    tryCatch(
      {
        gene_choices <- get_gene_choices(gene_data()$genes, input$check)

        updateSelectizeInput(
          session,
          "genes_n",
          label = if (isTRUE(input$check)) "Gene ID" else "Gene name",
          choices = gene_choices,
          server = TRUE
        )
      },
      error = function(e) {
        message("Error updating gene choices: ", e$message)
      }
    )
  })

  ## hide switch for tsne
  observeEvent(input$embedding, {
    if (input$embedding == "tsne") {
      runjs('$("#tab_switch").css("visibility", "hidden");')
    } else {
      runjs('$("#tab_switch").css("visibility", "visible");')
    }
  })

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

  ## automatically update size selection based on plot type
  observeEvent(input$plot,
    {
      if (!is.null(input$plot)) {
        if (input$plot == "2d") {
          updateRadioButtons(session, "size", selected = 2)
          runjs('$("input[name=\'size\'][value=\'2\']").prop("checked", true).trigger("change");')
        } else if (input$plot == "3d") {
          updateRadioButtons(session, "size", selected = 4)
          runjs('$("input[name=\'size\'][value=\'4\']").prop("checked", true).trigger("change");')
        }
      }
    },
    ignoreInit = TRUE
  )

  ## choose data based on input color
  data <- reactive({
    if (input$color == "genes") {
      filtered_gene_data()$data
    } else {
      filtered_data()
    }
  })

  # Add debouncing for plot updates
  plot_update_trigger <- reactiveVal(0)
  plot_ready <- reactiveVal(FALSE)

  # Debounced observer for plot-related inputs
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
      input$apply_filter
    )

    # Set plot as not ready immediately when inputs change
    tryCatch(
      {
        plot_ready(FALSE)
        autoUpdatePlot(FALSE)
      },
      error = function(e) {
        message("Error setting plot not ready: ", e$message)
      }
    )

    # Use later() to debounce plot updates - wait 1.2 seconds for all changes to settle
    later(
      function() {
        tryCatch(
          {
            isolate({
              plot_update_trigger(plot_update_trigger() + 1)
            })
          },
          error = function(e) {
            message("Error in plot update debounce: ", e$message)
          }
        )
      },
      1.2
    )
  })

  # Separate reactive to handle plot readiness
  observe({
    plot_update_trigger()

    # Additional delay to ensure data is fully loaded
    later(
      function() {
        tryCatch(
          {
            plot_ready(TRUE)
            autoUpdatePlot(TRUE)

            # Show completion notification for data type changes
            if (isolate(global_loading())) {
              showNotification(
                "Data type change completed!",
                type = "message",
                duration = 2
              )
              global_loading(FALSE)
            }
          },
          error = function(e) {
            message("Error in plot readiness: ", e$message)
            plot_ready(TRUE)
            autoUpdatePlot(TRUE)
          }
        )
      },
      0.3
    )
  })

  ## determine plot type
  get_plot_type <- reactive({
    switch(input$embedding,
      "pca" = if (isTRUE(input$check_all)) "pca_i" else "pca_u",
      "umap" = if (isTRUE(input$check_all)) "umap_i" else "umap_u",
      "tsne" = "tSNE"
    )
  })

  ## generate pca, umap and tsne plot with improved debouncing
  embplot <- reactiveVal(FALSE)
  output$all_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, plot_ready(), autoUpdatePlot())

    if (is.null(data()) || nrow(data()) == 0) {
      return(NULL)
    }

    # Generate plot without progress bar
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$genes),
        # "RNA_snn" = input$genes_n,
        input$color
      )

      # Validate color_column - if it's NA or NULL, fallback to a safe default
      if (is.null(color_column) || is.na(color_column) || color_column == "") {
        color_column <- "sample_type"
      }

      p <- embedding_plot(
        data(),
        input$xaxis,
        input$yaxis,
        color_column,
        input$genes_n,
        input$size,
        plot_type = get_plot_type(),
        plot_dim = input$plot
      )

      p <- p %>%
        event_register("plotly_click") %>%
        config(displaylogo = FALSE, scrollZoom = TRUE)

      embplot(TRUE)
      return(p)
    })
  }) ## click event: after plot is ready

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

    data <- filtered_gene_data()$data
    cell_id <- event$key

    # Process data without progress bar - keep it simple and fast
    data$id <- seq_len(nrow(data))
    cols <- c("id", colnames(data)[str_detect(colnames(data), "^PVP01")])
    selected_df <- data[data$id == cell_id, cols, drop = FALSE]

    # Clear intermediate variables
    rm(data)

    # Convert to long format using the original working approach
    selected_df <- selected_df %>%
      pivot_longer(
        cols = -id,
        names_to = "gene_id",
        values_to = "expression"
      ) %>%
      # Filter out zero expression genes for better performance
      filter(expression > 0) %>%
      left_join(gff_data, by = c("gene_id" = "id")) %>%
      arrange(desc(expression)) %>%
      rename(
        cell_id = id,
        gene_name = name,
        exp_value = expression
      ) %>%
      select(gene_id, gene_name, exp_value, description)

    selected_table(selected_df)

    showModal(modalDialog(
      title = paste("Selected Cell ID:", cell_id),
      div(style = "width: 100%;", DT::DTOutput("cell_expression_table")),
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

  # Clear selected table and cell cache when modal is dismissed (memory cleanup)
  observeEvent(input$shiny_modal, {
    if (!isTruthy(input$shiny_modal)) {
      selected_table(NULL)

      # Clear old cell cache entries to prevent memory buildup (keep last 10)
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
      paste("Plasmodium_vivax_data", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.4, detail = "Saving file...")
        write.csv(filtered_data(), file, row.names = FALSE)
        incProgress(1, detail = "Download complete")
      })
    }
  )

  # clear memory on session end with comprehensive cleanup
  onSessionEnded(function() {
    message("Starting session cleanup...")

    tryCatch(
      {
        # Remove UI elements
        # removeUI(selector = "#dynamic_col_*")

        # Clear reactive values safely
        if (exists("choice_exp")) {
          choice_exp(NULL)
        }
        if (exists("selected_table")) {
          selected_table(NULL)
        }
        if (exists("autoUpdatePlot")) {
          autoUpdatePlot(FALSE)
        }
        if (exists("app_initialized")) {
          app_initialized(FALSE)
        }
        if (exists("data_loading_trigger")) {
          data_loading_trigger(NULL)
        }
        if (exists("default_df_filter")) {
          default_df_filter(NULL)
        }
        if (exists("plot_ready")) {
          plot_ready(FALSE)
        }
        if (exists("global_loading")) {
          global_loading(FALSE)
        }

        # Clear all cached data explicitly (but preserve normalized data for speed)
        if (exists("cached_data")) {
          cached_data$raw <- NULL
          # cached_data$normalize <- NULL
          # cached_data$scale <- NULL
          cached_data$current_type <- NULL
          cached_data$current_filter <- NULL
        }

        # Clear session data
        if (exists("sessionData")) {
          if (exists("last_gene_data", envir = sessionData)) {
            sessionData$last_gene_data <- NULL
          }
          rm(list = ls(envir = sessionData), envir = sessionData)
        }

        # Clear temporary variables from global environment
        rm(
          list = ls(envir = .GlobalEnv, pattern = "^temp_"),
          envir = .GlobalEnv
        )
        message("Session ended. Memory cleanup completed.")
      },
      error = function(e) {
        message("Error during session cleanup: ", e$message)
      }
    )
  })

  # Enhanced function to manually clear cache with immediate garbage collection
  clear_data_cache <- function(force_gc = TRUE) {
    tryCatch(
      {
        message("Clearing data cache...")

        # Clear cached data (but preserve normalized data for gene expression speed)
        cached_data$raw <- NULL
        # cached_data$normalize <- NULL
        # cached_data$scale <- NULL
        cached_data$current_type <- NULL
        cached_data$current_filter <- NULL

        # Clear session data
        if (exists("sessionData")) {
          if (exists("last_gene_data", envir = sessionData)) {
            sessionData$last_gene_data <- NULL
          }

          # Clear cached filtered metadata and gene data
          cached_keys <- ls(envir = sessionData, pattern = "^filtered_")
          if (length(cached_keys) > 0) {
            rm(list = cached_keys, envir = sessionData)
          }
        }

        if (force_gc) {
          # Single garbage collection after cache clearing
          gc(verbose = FALSE)
        }

        message("Data cache cleared. Memory freed.")
      },
      error = function(e) {
        message("Error clearing cache: ", e$message)
      }
    )
  }
}
