server <- function(input, output, session) {
  sessionData <- new.env(parent = emptyenv())

  # Dynamic labels for gene selection
  output$genes_label_single <- renderText({
    if (isTRUE(input$check)) "Gene(s) id" else "Gene(s) name"
  })

  output$genes_label_first_comp <- renderText({
    if (isTRUE(input$check_first_comp)) "Gene(s) id" else "Gene(s) name"
  })

  output$genes_label_second <- renderText({
    if (isTRUE(input$check_second)) "Gene(s) id" else "Gene(s) name"
  })

  # Reactive value for comparison mode (toggled by action button)
  comparison_mode <- reactiveVal(FALSE)

  # Colour of the single plot, restored when leaving comparison mode
  ui_state <- reactiveValues(stored_color = get_data_column("broad_stage"))

  # Expression data of the session (server/data_loading.R)
  loaded <- data_loading_server(input, session)
  gene_data <- loaded$gene_data
  get_current_data_type <- loaded$data_type
  cached_data <- loaded$cached_data

  # Filter menus (server/filter_choices.R) and the filter state shared by the
  # tabs (server/shared_filters.R): tab_filters[[tab]]() gives the filter
  # values of a tab, debounced.
  filter_choices <- filter_choices_server(input, session, gene_data)
  tab_filters <- shared_filters_server(input, session, filter_choices)
  stage_filters_server(input, output, session)

  # Separate reactive for filtered gene data (Embeddings tab)
  filtered_gene_data <- reactive({
    req(gene_data())

    # Filters of the Embeddings tab; empty selections mean "no filter"
    current_inputs <- tab_filters[["Embeddings"]]()

    # Create cache key for filtered gene data
    cache_key <- make_cache_key(
      "filtered_gene_data",
      get_current_data_type(),
      current_inputs,
      list(exclude = attr(current_inputs, "exclude"))
    )

    # Check cache first to avoid reprocessing
    if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
      return(sessionData[[cache_key]])
    }

    # NOTE: no silent fallback to unfiltered data here. If filtering fails,
    # data and configuration do not match, and quietly showing the *unfiltered*
    # atlas as if the filters had been applied would be misleading.
    filtered_meta <- apply_secondary_filters(
      gene_data()$data,
      current_inputs,
      keep_gene_matrix = FALSE,
      genes = if (!is.null(gene_data()$genes)) gene_data()$genes$gene else NULL
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

  # Metadata plots tab: number boxes and the plot builders (server/tab_plots.R)
  plots_tab_server(input, output, reactive(mr_data), tab_filters[["Metadata plots"]])

  # Metadata table, QC Plots and Data sources tabs (server/tab_*.R)
  data_table_tab_server(output, tab_filters[["Metadata table"]])
  qc_tab_server(output, tab_filters[["QC Plots"]])
  data_sources_tab_server(input, output)

  # Gene expression tab and its cards (server/tab_gene_expression.R)
  gene_expression_tab_server(input, output, session, loaded, tab_filters[["Gene expression"]])

  # Gene menu of the single plot, when it is coloured by genes: choices as
  # names or IDs (input$check)
  observeEvent(list(input$color, input$check),
    {
      if (!identical(input$color, "genes")) {
        return()
      }

      req(gene_data()$genes)

      tryCatch(
        {
          genes_df <- gene_data()$genes
          if (is.null(genes_df) || nrow(genes_df) == 0) {
            return()
          }

          gene_choices <- get_gene_choices(genes_df, input$check)

          if (is.null(gene_choices) || length(gene_choices) == 0) {
            return()
          }

          # Keep the selection, as names or IDs like the menu
          current_selection <- convert_gene_selection(input$genes_n, genes_df, isTRUE(input$check))
          gene_choices <- c(gene_choices, setdiff(current_selection, gene_choices))

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
        },
        error = function(e) {
          message("Error updating gene choices: ", e$message)
        }
      )
    },
    ignoreInit = FALSE
  )

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
        genes_df <- gene_data()$genes
        # Keep the selection, as names or IDs like the menu
        selected <- convert_gene_selection(input$genes_n_first_comp, genes_df, isTRUE(input$check_first_comp))
        choices <- get_gene_choices(genes_df, isTRUE(input$check_first_comp))
        choices <- c(choices, setdiff(selected, choices))
        updateSelectizeInput(session, "genes_n_first_comp", choices = choices, selected = selected, server = TRUE)
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
    color_val <- if (!is.null(input$color_first_comp)) input$color_first_comp else get_data_column("broad_stage")
    genes_val <- if (!is.null(input$genes_n_first_comp)) input$genes_n_first_comp else ""
    check_val <- if (!is.null(input$check_first_comp)) input$check_first_comp else FALSE

    list(
      color = color_val,
      genes = genes_val,
      check = check_val,
      combine = input$combine_genes_first_comp,
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

    color_val <- if (!is.null(input$color_second)) input$color_second else get_data_column("detailed_stage")
    genes_val <- if (!is.null(input$genes_second)) input$genes_second else ""
    check_val <- if (!is.null(input$check_second)) input$check_second else FALSE

    list(
      color = color_val,
      genes = genes_val,
      check = check_val,
      combine = input$combine_genes_second,
      xaxis = input$xaxis,
      yaxis = input$yaxis,
      size = if (!is.null(input$size)) input$size else 4,
      plot = if (!is.null(input$plot)) input$plot else "3d",
      embedding = input$embedding,
      integration = input$check_all,
      timestamp = Sys.time()
    )
  })

  # The single plot is redrawn only once its inputs have not changed for 0.6 s
  plot_ready <- reactiveVal(TRUE)
  observe({
    list(
      input$embedding,
      input$plot,
      input$xaxis,
      input$yaxis,
      input$color,
      input$size,
      input$genes_n,
      input$combine_genes,
      input$data_type,
      input$check,
      input$check_all,
      lapply(filter_field_keys(), function(k) input[[k]])
    )

    plot_ready(FALSE)
    later(function() plot_ready(TRUE), 0.6)
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

    # Inputs still changing
    if (!plot_ready()) {
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


  # Embedding plot with the given colour options. The comparison plots
  # (is_dual_plot) use the gene tables cached with their data (data_source);
  # the single plot waits until its inputs are stable.
  generate_embedding_plot <- function(color_input, genes_input, check_input, xaxis_input, yaxis_input,
                                      size_input, plot_input, plot_type_input, is_dual_plot = FALSE,
                                      data_source = NULL, combine_input = "mean") {
    if (!is_dual_plot) {
      req(all_dependencies_ready(), embedding_ready())
    }
    plot_data <- if (!is.null(data_source)) data_source else data()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    isolate({
      if (isTRUE(color_input == "genes")) {
        gene_names <- attr(plot_data, "cached_genes")
        full_mat <- attr(plot_data, "cached_full_data")
        if (is.null(gene_names)) gene_names <- tryCatch(gene_data()$genes, error = function(e) NULL)
        if (is.null(full_mat)) full_mat <- cached_data[[get_current_data_type()]]
        if (is.null(full_mat)) full_mat <- gene_data()$data
        expression <- embedding_gene_expression(genes_input, check_input, gene_names, full_mat,
                                                attr(plot_data, "row_idx"))
        if (is.null(expression)) {
          return(NULL)
        }
        p <- embedding_gene_plot(plot_data, expression, combine_input, xaxis_input, yaxis_input,
                                 size_input, plot_type_input, plot_input, is_dual_plot)
      } else {
        p <- embedding_plot(plot_data, xaxis_input, yaxis_input, color_input,
                            get_label_for_column(color_input), size_input,
                            plot_type = plot_type_input, plot_dim = plot_input,
                            is_dual_plot = is_dual_plot)
      }
      embedding_plot_config(p)
    })
  }

  # Handle Compare Plots button click (toggle comparison mode)
  observeEvent(input$enable_comparison, {
    new_state <- !comparison_mode()
    comparison_mode(new_state)

    # Update hidden checkbox for conditionalPanel
    updateCheckboxInput(session, "comparison_active", value = new_state)

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
      shinyjs::hide("genes_single_panel")

      # Initialize Bootstrap tooltips
      shinyjs::runjs("var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')); var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) { return new bootstrap.Tooltip(tooltipTriggerEl); });")

      ui_state$stored_color <- input$color

      # Initialize comparison plots with same settings as main plot
      updateSelectInput(session, "color_first_comp", selected = input$color)
      updateSelectInput(session, "color_second", selected = input$color)

      # If using genes, copy gene selection
      if (!is.null(input$color) && input$color == "genes" && !is.null(input$genes_n) && length(input$genes_n) > 0) {
        # as names or IDs, like each comparison menu
        genes_df <- gene_data()$genes
        genes_a <- convert_gene_selection(input$genes_n, genes_df, isTRUE(input$check_first_comp))
        genes_b <- convert_gene_selection(input$genes_n, genes_df, isTRUE(input$check_second))
        later(function() {
          updateSelectizeInput(session, "genes_n_first_comp", selected = genes_a)
          updateSelectizeInput(session, "genes_second", selected = genes_b)
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

  # "Combine genes" menu of each embedding plot: "Each gene separately" is a
  # choice only for 2 or 3 genes. The menu is hidden for fewer than 2 genes
  # and keeps its value (the selection is briefly empty while the name/ID
  # switch converts it).
  for (ids in list(c("genes_n", "combine_genes"), c("genes_n_first_comp", "combine_genes_first_comp"),
                   c("genes_second", "combine_genes_second"))) {
    local({
      genes_id <- ids[[1]]
      combine_id <- ids[[2]]
      observeEvent(length(input[[genes_id]]), {
        n_genes <- length(input[[genes_id]])
        req(n_genes >= 2)
        updateSelectizeInput(session, combine_id, choices = gene_combine_choices(n_genes),
                             selected = effective_gene_combine(input[[combine_id]], n_genes))
      })
    })
  }

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

          # Keep the selection, as names or IDs like the menu
          current_selection_second <- convert_gene_selection(input$genes_second, genes_df, isTRUE(input$check_second))
          gene_choices <- c(gene_choices, setdiff(current_selection_second, gene_choices))

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
        },
        error = function(e) {
          message("Error updating second plot gene choices: ", e$message)
        }
      )
    },
    ignoreInit = FALSE
  )

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
          gene_links_multiple(gene_ids, target = "plasmo_link_a")
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
          gene_links_multiple(gene_ids, target = "plasmo_link_b")
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
      data_source = first_plot_data(),
      combine_input = plot_params$combine
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
      data_source = second_plot_data(),
      combine_input = plot_params$combine
    )
  })

  # Single embedding plot, shown when comparison mode is off
  output$all_plotly <- renderPlotly({
    req(!isTRUE(comparison_mode()))
    req(all_dependencies_ready(), embedding_ready())
    ptype <- get_plot_type()
    p <- isolate(generate_embedding_plot(
      input$color, input$genes_n, input$check, input$xaxis, input$yaxis, input$size, input$plot,
      plot_type_input = ptype, combine_input = input$combine_genes
    ))
    embplot(TRUE)
    p
  })

  ## Cell details: opened by a double-click or right-click on a cell of an
  ## embedding plot (input$embedding_cell_open, set in www/script.js)
  click_event <- reactive({
    req(embplot())
    ed <- input$embedding_cell_open
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
        write.csv(mr_data, file, row.names = FALSE)
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
      },
      error = function(e) {}
    )
  })
}
