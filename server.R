library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(later)

# Load raw and scale data.
raw_df <- readRDS("data/raw_df.rds")
scale_df <- readRDS("data/scale_df.rds")

server <- function(input, output, session) {
  sessionData <- new.env(parent = emptyenv())
  autoUpdatePlot <- reactiveVal(FALSE)

  # Navigate home and data section
  observeEvent(input$home, {
    shinyjs::hide("analysis_section")
    shinyjs::hide("about_section")
    shinyjs::show("home_section")

    shinyjs::runjs("
    $('.menu-link').removeClass('active');
    $('#home').addClass('active');
  ")
  })

  observeEvent(list(input$about, input$contribute_link, input$about_link),
    {
      shinyjs::hide("home_section")
      shinyjs::hide("analysis_section")
      shinyjs::show("about_section")

      runjs("initParticles('colab-particles');")
      runjs("initParticles('particles-js-about');")


      shinyjs::runjs("
    $('.menu-link').removeClass('active');
    $('#about').addClass('active');
  ")
    },
    ignoreInit = TRUE
  )

  observeEvent(input$analysis, {
    shinyjs::hide("home_section")
    shinyjs::hide("about_section")
    shinyjs::show("analysis_section")

    shinyjs::runjs("
    $('.menu-link').removeClass('active');
    $('#analysis').addClass('active');
  ")
  })

  # Navigate tabsetPannel
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

  observeEvent(input$usermanual_link, {
    runjs("window.open('usermanual.html', '_blank')")
  })


  # Larger screen adjustment
  observe({
    if (input$tabset == "Metadata" ||
      input$tabset == "Quality control" || input$tabset == "Data sources" ||
      input$tabset == "Gene expression") {
      shinyjs::removeClass("sidebar", "visible")
      shinyjs::runjs('$("#main").addClass("col-sm-12");')
    } else {
      shinyjs::addClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-12");')
    }
  })


  # Change to normalize when change tabset
  observeEvent(input$tabset, {
    if (input$tabset == "Gene expression") {
      updateRadioButtons(session, "data_type_exp",
        choices = list(
          "Raw" = "raw",
          "Normalized" = "normalize",
          "Scaled" = "scale"
        ),
        selected = "normalize"
      )
    } else if (input$tabset == "Embeddings") {
      updateRadioButtons(session, "data_type",
        choices = list(
          "Raw" = "raw",
          "Normalized" = "normalize",
          "Scaled" = "scale"
        ),
        selected = "normalize"
      )
    }
  })


  # Select raw, normalize or scale data
  default_df_filter <- reactiveVal(NULL)

  gene_data <- eventReactive(
    list(input$tabset, input$apply_filter, input$data_type, input$data_type_exp),
    {
      dataset <- if (input$tabset == "Gene expression") {
        req(input$data_type_exp)
        switch(input$data_type_exp,
          raw = raw_df,
          normalize = norm_df,
          scale = scale_df
        )
      } else if (input$tabset == "Embeddings") {
        req(input$data_type)
        switch(input$data_type,
          raw = raw_df,
          normalize = norm_df,
          scale = scale_df
        )
      }

      req(dataset)
      selections <- input$apply_filter

      if (user_interact() && (is.null(selections) || length(selections) == 0)) {
        return(NULL)
      }

      if (is.null(selections)) {
        selections <- default_df_filter()
      }


      req(selections)
      list(
        data = fun_filter_df(dataset, selections),
        genes = genes(dataset, gff_data)
      )
    }
  )

  # Metadata filter
  # choose columns for filter data
  grouped_choices <- list(
    stage = setNames(
      paste0("stage__", unique(mr_data$stage)),
      unique(mr_data$stage)
    ),
    species = setNames(
      paste0("species__", unique(mr_data$species)),
      unique(mr_data$species)
    ),
    reproductive_form = setNames(
      paste0("type__", unique(mr_data$reproductive_form)),
      unique(mr_data$reproductive_form)
    ),
    condition = setNames(
      paste0("condition__", unique(mr_data$condition)),
      unique(mr_data$condition)
    )
  )

  observe({
    first_group <- names(grouped_choices)[1]
    default_values <- grouped_choices[[first_group]]

    if (is.null(default_df_filter())) {
      default_df_filter(default_values)
    }

    updateSelectInput(session, "apply_filter",
      choices = grouped_choices,
      selected = default_values
    )
  })

  user_interact <- reactiveVal(FALSE)
  observeEvent(input$apply_filter, {
    user_interact(TRUE)
  })

  # Reactive filtered data
  filtered_data <- reactive({
    selections <- input$apply_filter

    if (user_interact() && (is.null(selections) || length(selections) == 0)) {
      return(data.frame())
    }

    if (is.null(selections)) {
      selections <- default_df_filter()
    }

    req(selections)
    fun_filter_df(mr_data, selections)
  })



  # Manipulation tabset "metadata"
  observe({
    filt_data <- filtered_data()
    req(nrow(filt_data) > 0)

    if (input$tabset == "Metadata") {
      updateSelectInput(session, "ident", choices = unique(filt_data$ident))
      updateSelectInput(session, "species", choices = unique(filt_data$species))
      updateSelectInput(session, "repro_form", choices = unique(filt_data$reproductive_form))
      updateSelectInput(session, "condition", choices = unique(filt_data$condition))
      updateSelectInput(session, "stage", choices = unique(filt_data$stage))
      updateSelectInput(session, "sample", choices = unique(filt_data$sample))
    }
  })


  observeEvent(list(
    input$data_opt, input$ident, input$species, input$repro_form,
    input$condition, input$stage, input$sample
  ), {
    filt_data <- filtered_data() %>%
      select(any_of(c(
        "id", "ident", "seurat_clusters", "species", "reproductive_form",
        "condition", "stage", "sample", "day", "replicate",
        "location", "vivax_strain", "n_count_rna", "n_feature_rna"
      )))

    if (!is.null(input$ident) || !is.null(input$species) ||
      !is.null(input$repro_form) || !is.null(input$condition) ||
      !is.null(input$stage) || !is.null(input$sample)) {
      filt_data <- filt_data %>%
        filter(
          if (!is.null(input$ident)) ident %in% input$ident else TRUE,
          if (!is.null(input$species)) species %in% input$species else TRUE,
          if (!is.null(input$repro_form)) reproductive_form %in% input$repro_form else TRUE,
          if (!is.null(input$condition)) condition %in% input$condition else TRUE,
          if (!is.null(input$stage)) stage %in% input$stage else TRUE,
          if (!is.null(input$sample)) sample %in% input$sample else TRUE
        )
    }

    output$data_table <- renderDT(
      {
        ## data in table
        DT::datatable(
          filt_data %>%
            rename_with(~ str_to_title(.),
              .cols = any_of(c(
                "id", "ident", "seurat_clusters",
                "species", "reproductive_form", "condition", "stage",
                "sample", "day", "replicate", "location",
                "vivax_strain"
              ))
            ) %>%
            rename(nCount_RNA = n_count_rna, nFeature_RNA = n_feature_rna) %>%
            arrange(desc(nCount_RNA)),
          options = list(pageLength = 10, scrollX = TRUE),
          selection = "single",
          filter = "top",
          rownames = FALSE
        )
      },
      server = TRUE
    )

    ## plot stage vs count
    output$stage_plotly <- renderPlotly({
      meta_plotly(filt_data, "stage", "Count", "Stage", "bar")
    })

    ## plot sample vs count
    output$sample_plotly <- renderPlotly({
      meta_plotly(filt_data, "sample", "Count", "Sample", "bar")
    })

    ## plot strain vs count
    output$strain_plotly <- renderPlotly({
      meta_plotly(filt_data, "vivax_strain", "Count", "Strain", "bar")
    })

    ## plot species vs count
    output$species_plotly <- renderPlotly({
      meta_plotly(filt_data, "species", "Count", "Species", "bar")
    })

    ## plot reproductive_form vs count
    output$type_plotly <- renderPlotly({
      meta_plotly(filt_data, "reproductive_form", "Count", "Reproductive Form", "bar")
    })

    ## plot location vs count
    output$location_plotly <- renderPlotly({
      meta_plotly(filt_data, "location", "Count", "Location", "bar")
    })
  })

  # Quality control tabset
  observeEvent(input$tabset, {
    if (input$tabset == "Quality control") {
      ## plot nCount in qual tabset
      output$data_count_plotly <- renderPlotly({
        feature_plot(filtered_data(),
          x = "sample", y = "n_count_rna",
          color = "ident", y_title = "RNA count", title = "RNA Count"
        )
      })

      ## plot nFeature in qual tabset
      output$data_feature_plotly <- renderPlotly({
        feature_plot(filtered_data(),
          x = "sample", y = "n_feature_rna",
          color = "ident", y_title = "nFeature count",
          title = "Feature Count"
        )
      })

      ## plot elbow
      output$data_pca_stdv_plotly <- renderPlotly({
        elbow_plot(pca_std_df)
      })

      ## plot variable genes
      output$variable_genes_plotly <- renderPlotly({
        var_genes_plot(vf_data, top_n = 15)
      })
    }
  })


  # Data sources tabset
  observeEvent(input$tabset, {
    if (input$tabset == "Data sources") {
      output$data_source <- renderDT({
        DT::datatable(pub_data,
          escape = FALSE,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = FALSE,
            columnDefs = list(
              list(width = "800px", targets = 3, className = "dt-left")
            )
          ),
          selection = "single",
          rownames = FALSE
        )
      })
    }
  })



  # Manipulation tabset "Gene Exp"
  ## gene_exp_0 block
  choice_exp_0 <- reactive({
    get_gene_choices(gene_data()$genes, input$check_exp_0)
  })

  observe({
    req(choice_exp_0())
    updateSelectizeInput(session, "genes_exp_0",
      label = if (isTRUE(input$check_exp_0)) "Gene ID" else "Gene name",
      choices = choice_exp_0(),
      server = TRUE
    )
  })

  ## df_exp data for genes_exp_0
  df_exp_0 <- reactive({
    req(gene_data(), input$genes_exp_0, input$group_exp)
    df_exp0(
      gene_data = gene_data(),
      gene_input = input$genes_exp_0,
      group_input = input$group_exp,
      check_exp = input$check_exp_0
    )
  })


  ## plot for genes_exp_0
  output$gene_exp_plotly <- renderPlotly({
    req(df_exp_0())
    group <- isolate(tolower(input$group_exp))
    gene_exp_plot(df_exp_0(), group)
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
  observeEvent(input$click_add_btn, {
    btn_id <- input$click_add_btn
    id <- as.numeric(gsub("add_btn_?", "", btn_id))
    shinyjs::hide(paste0("add_btn_", id))
    df_exp_i <- df_expi(id, input, gene_data(), get_gene_id)

    choice_exp(get_gene_choices(gene_data()$genes, FALSE))

    ## add ui block to the right after gene_exp_tar
    if (id == 0) {
      id <- id + 1
      insertUI(
        selector = "#gene_exp_tar",
        where = "afterEnd",
        ui = column(
          width = 6,
          id = paste0("dynamic_col_", id),
          subBlock(id)
        )
      )
      shinyjs::delay(100, {
        updateSelectizeInput(
          session,
          inputId = paste0("genes_exp_", id),
          choices = choice_exp(),
          server = TRUE
        )
      })
      output[[paste0("gene_exp_plotly_", id)]] <- renderPlotly({
        req(df_exp_i())
        gene_exp_plot(
          df_exp_i(),
          tolower(input[[paste0("group_exp_", id)]])
        )
      })
    } else {
      ## add ui block to button
      id <- id + 1
      newBlock(id, counter, choice_exp(), subBlock, session)
      output[[paste0("gene_exp_plotly_", id)]] <- renderPlotly({
        req(df_exp_i())
        gene_exp_plot(
          df_exp_i(),
          tolower(input[[paste0("group_exp_", id)]])
        )
      })
    }
  })

  ## observe remove button click from js
  observeEvent(input$click_remove_btn,
    {
      rm_btn_id <- input$click_remove_btn$btn_id
      id <- as.numeric(gsub("remove_btn_", "", rm_btn_id))
      ids <- as.numeric(unlist(input$click_remove_btn$ids))
      removeUI(selector = paste0("#dynamic_col_", id))
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

  ## update plot when data_type_exp_change from js
  observeEvent(input$data_type_exp_change, {
    req(gene_data(), input$data_type_exp_change)

    for (full_id in input$data_type_exp_change$ids) {
      id <- sub("^genes_exp_", "", full_id)
      group_id <- paste0("group_exp_", id)

      output[[paste0("gene_exp_plotly_", id)]] <- renderPlotly({
        req(gene_data(), input[[full_id]], input[[group_id]])

        df <- df_expi(
          id = id,
          input = input,
          gene_data = gene_data(),
          get_gene_id = get_gene_id
        )
        gene_exp_plot(df(), tolower(input[[group_id]]))
      })
    }
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
    get_gene_choices(gene_data()$genes, input$check)
  })

  ## dynamic UI for gene and cca_snn selection
  shinyjs::hide("gene_snn_ui")
  shinyjs::hide("data_type")
  observe({
    if (input$color == "genes") {
      shinyjs::show("data_type")
      shinyjs::show("gene_snn_ui")
      shinyjs::show("gene_link_ui")
      shinyjs::show("switch_id")
      updateSelectizeInput(session, "genes_n",
        label = if (isTRUE(input$check)) "Gene ID" else "Gene name",
        choices = choice(),
        server = TRUE
      )
    } else if (input$color == "cca_snn") {
      shinyjs::show("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
      shinyjs::hide("data_type")
      shinyjs::hide("switch_id")
      updateSelectizeInput(session, "genes_n",
        label = "Select Resolution",
        choices = cca_snn,
        server = TRUE
      )
    } else {
      shinyjs::hide("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
      shinyjs::hide("data_type")
      shinyjs::hide("switch_id")
    }
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
  observeEvent(list(
    input$embedding, input$plot, input$xaxis, input$check_all
  ), {
    if (input$plot == "2d") {
      shinyjs::show("axis")
      x_choices <- switch(input$embedding,
        "umap" = if (isTRUE(input$check_all)) umap_i else umap_u,
        "pca" = if (isTRUE(input$check_all)) pca_i else pca_u,
        "tsne" = tsne
      )

      updateSelectInput(session, "xaxis",
        choices = x_choices,
        selected = x_choices[1]
      )

      y_choices <- switch(input$embedding,
        "umap" = y_col(
          input$check_all, input$xaxis, umap_i, umap_u
        ),
        "pca" = y_col(input$check_all, input$xaxis, pca_i, pca_u),
        "tsne" = setdiff(tsne, input$xaxis)
      )

      updateSelectInput(session, "yaxis",
        choices = y_choices,
        selected = if (input$yaxis %in% y_choices) input$yaxis else y_choices[2]
      )
    } else {
      shinyjs::hide("axis")
    }
  })

  ## choose data based on input color
  data <- reactive({
    if (input$color == "genes") {
      gene_data()$data
    } else {
      filtered_data()
    }
  })

  ## update after 1 sec
  observe({
    list(
      input$embedding, input$plot, input$xaxis, input$yaxis, input$color,
      input$size, input$genes_n, input$data_type, input$check,
      input$check_all, input$apply_filter
    )
    autoUpdatePlot(FALSE)
    later(function() {
      autoUpdatePlot(TRUE)
    }, 1)
  })

  ## determine plot type
  get_plot_type <- reactive({
    switch(input$embedding,
      "pca" = if (isTRUE(input$check_all)) "pca_i" else "pca_u",
      "umap" = if (isTRUE(input$check_all)) "umap_i" else "umap_u",
      "tsne" = "tSNE"
    )
  })


  ## generate pca, umap and tsne plot
  embplot <- reactiveVal(FALSE)
  output$all_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, autoUpdatePlot())


    if (is.null(data()) || nrow(data()) == 0) {
      return(NULL)
    }


    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$genes),
        "cca_snn" = input$genes_n,
        input$color
      )
      p <- embedding_plot(
        data(), input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size,
        plot_type = get_plot_type(), plot_dim = input$plot
      )
      p <- p %>%
        event_register("plotly_click") %>%
        config(displaylogo = FALSE, scrollZoom = TRUE)
      embplot(TRUE)
      return(p)
    })
  })

  ## click event: after plot is ready
  click_event <- reactive({
    req(embplot())
    ed <- event_data("plotly_click", source = "embedding")
    if (is.null(ed) || is.null(ed$key)) {
      return(NULL)
    }
    return(ed)
  })

  ## table on cell click
  selected_table <- reactiveVal(NULL)
  observeEvent(click_event(), {
    event <- click_event()
    req(event)
    data <- gene_data()$data
    cell_id <- event$key
    cols <- c("id", colnames(data)[str_detect(colnames(data), "^PVP01")])
    selected_df <- data[data$id == cell_id, cols, drop = FALSE]

    selected_df <- selected_df %>%
      pivot_longer(
        cols = -id,
        names_to = "gene_id",
        values_to = "expression"
      ) %>%
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
    datatable(selected_table(),
      options = list(
        pageLength = 10,
        scrollY = "400px",
        scrollCollapse = TRUE,
        paging = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      filter = "top",
      selection = "single",
      width = "100%"
    )
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

  # clear memory on session end
  onSessionEnded(function() {
    removeUI(selector = "#dynamic_col_*")
    choice_exp(NULL)
    selected_table(NULL)

    if (exists("sessionData", envir = .GlobalEnv)) {
      rm(list = ls(envir = sessionData), envir = sessionData)
    }
    rm(list = ls(envir = .GlobalEnv, pattern = "^temp_"), envir = .GlobalEnv)
    gc()
    message("Session ended. Memory cleanup completed.")
  })
}
