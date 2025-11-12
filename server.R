library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(later)

server <- function(input, output, session) {
  sessionData <- new.env(parent = emptyenv())

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

  # Synchronize data type selection across tabs
  observeEvent(input$tabset, {
    syncing_tabs(TRUE)

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
          ),
          selected = current_selection
        )
      }
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
      withProgress(message = "Loading data", value = 30, {
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

        # Cache the current state (only data type, no filter)
        cached_data$current_type <- current_type
        sessionData$last_gene_data <- result
        return(result)
      })
    },
    ignoreNULL = FALSE
  )

  # Separate reactive for filtered gene data
  filtered_gene_data <- reactive({
    req(gene_data())

    selections <- input$apply_filter

    # If no selections made (empty or null), use default filter
    if (is.null(selections) || length(selections) == 0) {
      selections <- default_df_filter()
    }

    req(selections)

    # Create cache key for filtered gene data
    cache_key <- make_cache_key(
      "filtered_gene_data",
      get_current_data_type(),
      selections
    )

    # Check cache first to avoid reprocessing
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }

    # subset that contains all gene columns (which caused large copies).
    parsed_inputs <- list()
    if (!is.null(selections) && length(selections) > 0) {
      parts <- strsplit(selections, "__", fixed = TRUE)
      valid_parts <- Filter(function(x) length(x) == 2, parts)
      if (length(valid_parts) > 0) {
        filter_list <- lapply(
          split(valid_parts, vapply(valid_parts, `[[`, character(1), 1)),
          function(lst) vapply(lst, `[[`, character(1), 2)
        )
        parsed_inputs <- filter_list
      }
    }

    filtered_meta <- tryCatch(
      {
        apply_secondary_filters(gene_data()$data, parsed_inputs, keep_gene_matrix = FALSE, genes = if (!is.null(gene_data()$genes)) gene_data()$genes$gene else NULL)
      },
      error = function(e) {
        # Fallback to safe fun_filter_df behavior if something unexpected
        fun_filter_df(gene_data()$data, selections)
      }
    )

    result <- list(
      data = filtered_meta,
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
    input_name = "data_type", # embeddings tabset
    target_input = "data_type_exp",
    expected_tabset = "Embeddings",
    global_loading = global_loading
  )

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

  # Reactive filtered data
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
      updateSelectizeInput(
        session,
        "host_species",
        choices = setNames(
          unique(filt_data$host_species),
          format_species_name(unique(filt_data$host_species))
        ),
        server = TRUE
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
      updateSelectizeInput(
        session,
        "host_species_qc",
        choices = setNames(
          unique(filt_data$host_species),
          format_species_name(unique(filt_data$host_species))
        ),
        server = TRUE
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
      updateSelectizeInput(
        session,
        "host_species_exp",
        choices = setNames(
          unique(filt_data$host_species),
          format_species_name(unique(filt_data$host_species))
        ),
        server = TRUE
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
            select(
              -c(
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
              )
            ) %>%
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


  # Gene Expression tabset
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

    # Create cache key as a single string for filtered gene expression data
    cache_key <- filtered_gene_exp_cache_key(input)
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }
    tryCatch(
      {
        # Get the full expression matrix from the cached_data storage where
        full_df <- if (!is.null(cached_data[[get_current_data_type()]])) cached_data[[get_current_data_type()]] else filtered_gene_data()$data

        filt_meta <- apply_secondary_filters(
          filtered_gene_data()$data,
          current_inputs,
          keep_gene_matrix = FALSE,
          genes = if (!is.null(filtered_gene_data()$genes)) filtered_gene_data()$genes$gene else NULL
        )

        # Ensure row indices are available on the metadata result
        row_idx <- attr(filt_meta, "row_idx")

        result <- list(
          data = filt_meta,
          genes = filtered_gene_data()$genes,
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



  # Manipulation tabset "Gene Exp"
  ## gene_exp_0 block with minimal caching for speed
  choice_exp_0 <- reactive({
    req(gene_data()$genes)
    get_gene_choices(gene_data()$genes, input$check_exp_0)
  })

  # df_exp data for genes_exp_0 with simplified reactive approach
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
          check_exp = input$check_exp_0,
          full_data = filtered_gene_exp_data()$full
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
    try(
      {
        if (!is.null(result)) {
          df_gene0(result)
        }
      },
      silent = TRUE
    )

    return(result)
  })

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

    tryCatch(
      {
        updateSelectizeInput(
          session,
          "genes_exp_0",
          label = if (isTRUE(input$check_exp_0)) "Gene ID" else "Gene name",
          choices = choices,
          server = TRUE
        )
      },
      error = function(e) {
        # Silent error handling
      }
    )
  })

  # df_exp_0 when the user selects a gene/group.
  output$gene_exp_plotly <- renderPlotly({
    req(input$genes_exp_0, input$group_exp)

    if (global_loading()) {
      return(NULL)
    }

    plot_data <- df_gene0()
    if (is.null(plot_data)) {
      plot_data <- df_exp_0()
      try(
        {
          df_gene0(plot_data)
        },
        silent = TRUE
      )
    }
    req(plot_data)

    if (nrow(plot_data) == 0 || !"gene_expr" %in% names(plot_data)) {
      return(NULL)
    }

    # Create cache key (for fast switching of gene/group)
    plot_cache_key <- make_cache_key(
      "gene_exp_plot",
      input$genes_exp_0,
      input$group_exp,
      input$study_label_exp,
      input$check_exp_0
    )

    if (exists(plot_cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[plot_cache_key]])
    }

    result <- tryCatch(
      {
        group <- tolower(input$group_exp)
        gene_exp_plot(plot_data, group)
      },
      error = function(e) NULL
    )

    if (!is.null(result)) sessionData[[plot_cache_key]] <- result
    return(result)
  })

  ## gene expression link
  observeEvent(input$genes_exp_0, {
    req(input$genes_exp_0)
    gene_id <- get_gene_id(input$genes_exp_0, gene_data()$genes)
    gene_links(gene_id)
  })

  # gene_exp_i block
  # observe add button click from js
  choice_exp <- reactiveVal()
  counter <- reactiveVal(0)
  observeEvent(input$click_add_btn, {
    btn_id <- input$click_add_btn
    id <- as.numeric(gsub("add_btn_?", "", btn_id))
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
              updateSelectizeInput(
                session,
                inputId = paste0("genes_exp_", id),
                choices = selectize_choices,
                server = TRUE
              )
            }
          },
          error = function(e) {
            message("Error updating selectize input for dynamic gene: ", e$message)
          }
        )
      }, 0.25)

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
              cur_gene <- isolate(input[[paste0("genes_exp_", plot_id)]])
              cur_group <- isolate(input[[paste0("group_exp_", plot_id)]])
              cur_check <- isolate(input[[paste0("check_exp_", plot_id)]])

              # Resolve gene id when the user selects by name
              gene_exp_val <- if (isTRUE(cur_check)) cur_gene else get_gene_id(cur_gene, fg$genes)

              static_df <- tryCatch(
                {
                  build_plot_slim(fg$data, fg$full, gene_exp_val, cur_group)
                },
                error = function(e) {
                  message(sprintf("dynamic gene plot %s: error building slim df: %s", plot_id, e$message))
                  NULL
                }
              )

              if (is.null(static_df) || nrow(static_df) == 0) {
                message(sprintf("dynamic gene plot %s: no data after building slim df (gene=%s)", plot_id, as.character(gene_exp_val)))
                return(plotly::plot_ly() %>% plotly::layout(title = "No data"))
              }

              gene_exp_plot(static_df, tolower(cur_group))
            },
            error = function(e) {
              message(
                "Error in dynamic gene expression plot ",
                plot_id,
                ": ",
                e$message
              )
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
              updateSelectizeInput(
                session,
                inputId = paste0("genes_exp_", id),
                choices = selectize_choices,
                server = TRUE
              )
            }
          },
          error = function(e) {
            message("Error updating selectize input for dynamic gene: ", e$message)
          }
        )
      }, 0.25)

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
              cur_gene <- isolate(input[[paste0("genes_exp_", plot_id)]])
              cur_group <- isolate(input[[paste0("group_exp_", plot_id)]])
              cur_check <- isolate(input[[paste0("check_exp_", plot_id)]])

              gene_exp_val <- if (isTRUE(cur_check)) cur_gene else get_gene_id(cur_gene, fg$genes)

              static_df <- tryCatch(
                {
                  build_plot_slim(fg$data, fg$full, gene_exp_val, cur_group)
                },
                error = function(e) {
                  message(sprintf("dynamic gene plot %s: error building slim df: %s", plot_id, e$message))
                  NULL
                }
              )

              if (is.null(static_df) || nrow(static_df) == 0) {
                message(sprintf("dynamic gene plot %s: no data after building slim df (gene=%s)", plot_id, as.character(gene_exp_val)))
                return(plotly::plot_ly() %>% plotly::layout(title = "No data"))
              }

              gene_exp_plot(static_df, tolower(cur_group))
            },
            error = function(e) {
              message(
                "Error in dynamic gene expression plot ",
                plot_id,
                ": ",
                e$message
              )
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

  choice <- eventReactive(list(input$check, input$data_type), {
    req(gene_data()$genes)
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

      # Set loading state when showing gene UI
      genes_loading(TRUE)

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

          # Reset loading state after update
          later(
            function() {
              genes_loading(FALSE)
            },
            0.1
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
          genes_loading(FALSE)
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
      genes_loading(FALSE)
    }
  })

  # Separate observer to update gene choices when data or toggle changes
  observe({
    req(gene_data()$genes, input$color == "genes")

    # Set loading state to prevent plot updates
    genes_loading(TRUE)

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
    if (input$color == "genes") {
      filtered_gene_data()$data
    } else {
      filtered_data()
    }
  })

  # Add debouncing for plot updates
  plot_update_trigger <- reactiveVal(0)
  plot_ready <- reactiveVal(FALSE)
  genes_loading <- reactiveVal(FALSE)
  autoUpdatePlot <- reactiveVal(FALSE)

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
            message("Error in plot update debounce: ", e$message)
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
    switch(input$embedding,
      "pca" = if (isTRUE(input$check_all)) "pca_i" else "pca_u",
      "umap" = if (isTRUE(input$check_all)) "umap_i" else "umap_u",
      "tsne" = "tSNE"
    )
  })

  ## generate pca, umap and tsne plot with comprehensive dependency checking
  embplot <- reactiveVal(FALSE)

  # Embedding plot readiness state
  embedding_ready <- reactiveVal(FALSE)
  last_gene_selection <- reactiveVal("")

  # Comprehensive dependency checker with gene selection stability
  all_dependencies_ready <- reactive({
    if (is.null(data()) || nrow(data()) == 0) {
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
      if (is.null(current_gene) || current_gene == "") {
        return(FALSE)
      }

      # Check if gene selection has been stable
      if (last_gene_selection() != current_gene) {
        last_gene_selection(current_gene)
        invalidateLater(150)
        return(FALSE)
      }
    }
    return(TRUE)
  })

  # Observer to set ready state when dependencies are met
  observe({
    embedding_ready(all_dependencies_ready())
  })

  output$all_plotly <- renderPlotly({
    req(all_dependencies_ready(), embedding_ready())

    if (is.null(data()) || nrow(data()) == 0) {
      return(NULL)
    }

    # Generate plot without progress bar
    symbol_choice <- NULL
    if (!is.null(input$symbol)) {
      sym_val <- as.character(input$symbol)
      if (!is.na(sym_val)) {
        sym_val <- trimws(sym_val)
        if (nzchar(sym_val) && tolower(sym_val) != "unselected") {
          symbol_choice <- sym_val
        } else {
          symbol_choice <- NULL
        }
      }
    }

    isolate({
      df_local <- data()

      color_column <- NULL

      if (isTRUE(input$color == "genes")) {
        # Resolve gene input robustly: user may supply a display name or a gene id
        gene_input <- input$genes_n
        gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
        full_mat <- if (!is.null(cached_data[[get_current_data_type()]])) cached_data[[get_current_data_type()]] else gene_data()$data
        row_idx <- attr(df_local, "row_idx")

        resolved_gene_col <- NULL

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

        # Attach temporary gene color column if we have a valid resolved column and row indices
        if (!is.null(resolved_gene_col) && !is.na(resolved_gene_col) && !is.null(full_mat) && resolved_gene_col %in% colnames(full_mat) && !is.null(row_idx) && length(row_idx) > 0) {
          gene_color_col <- paste0(".gene_color_", gsub("[^A-Za-z0-9]", "_", resolved_gene_col))
          df_local[[gene_color_col]] <- full_mat[[resolved_gene_col]][row_idx]
          color_column <- gene_color_col
        } else {
          color_column <- "sample_type"
        }
      } else {
        color_column <- input$color
      }

      # Final safety check: ensure the resolved color column exists in df_local
      if (is.null(color_column) || is.na(color_column) || color_column == "" || !(color_column %in% colnames(df_local))) {
        color_column <- "sample_type"
      }

      p <- embedding_plot(
        df_local,
        input$xaxis,
        input$yaxis,
        color_column,
        input$genes_n,
        input$size,
        plot_type = get_plot_type(),
        plot_dim = input$plot,
        symbol = symbol_choice
      )

      p <- p %>%
        event_register("plotly_click") %>%
        config(displaylogo = FALSE, scrollZoom = TRUE)

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
        div(style = "width: 100%;", DT::DTOutput("cell_expression_table")),
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
    # mapped full row. Fall back to the original cell_key if barcode is missing.
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

        message("Session ended. Memory cleaned.")
      },
      error = function(e) {
        message("Error during cleanup: ", e$message)
      }
    )
  })
}
