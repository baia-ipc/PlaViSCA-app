# load libraries
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(later)

# read expression data
exp_df <- readRDS("data/expression_data.rds")
norm_df <- exp_df$normalize_df
raw_df <- exp_df$raw_df
scale_df <- exp_df$scale_df
rm(exp_df)

server <- function(input, output, session) {
  sessionData <- new.env(parent = emptyenv())
  autoUpdatePlot <- reactiveVal(FALSE)

  # navigate home and data section
  observeEvent(input$explore_data, {
    shinyjs::hide("home_section")
    shinyjs::show("data_sets_section")
  })

  observeEvent(input$data, {
    shinyjs::hide("home_section")
    shinyjs::show("data_sets_section")
  })

  observeEvent(input$home, {
    shinyjs::hide("data_sets_section")
    shinyjs::show("home_section")
  })

  # adjust css class for data and Gene Exp tabset
  observe({
    if (input$tabset == "Data" || input$tabset == "Gene Exp") {
      shinyjs::removeClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-8").addClass("col-sm-11");')
    } else {
      shinyjs::addClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-11").addClass("col-sm-8");')
    }
  })

  # select and filter data (raw, normalized and scaled)
  gene_data <- eventReactive(list(
    input$stagei, input$data_type,
    input$data_type_exp
  ), {
    req(input$stagei)
    dataset <- switch(input$data_type,
      "raw" = raw_df,
      "normalize" = norm_df,
      "scale" = scale_df
    )

    dataset <- switch(input$data_type_exp,
      "raw" = raw_df,
      "normalize" = norm_df,
      "scale" = scale_df
    )

    list(
      data = dataset %>% filter(stage %in% input$stagei),
      genes = genes(dataset, gff_data)
    )
  })

  # metadata filter
  filtered_data <- eventReactive(input$stagei, {
    req(input$stagei)
    mr_data %>% filter(stage %in% input$stagei)
  })

  # data based on input color
  data <- reactive({
    if (input$color == "genes") {
      gene_data()$data
    } else {
      filtered_data()
    }
  })

  # Data tabset
  observeEvent(input$tabset, {
    if (input$tabset == "Data") {
      filt_data <- filtered_data()
      updateSelectInput(session, "ident", choices = unique(filt_data$ident))
      updateSelectInput(session, "species", choices = unique(filt_data$species))
      updateSelectInput(session, "type", choices = unique(filt_data$type))
      updateSelectInput(session, "condition", choices = unique(filt_data$condition))
      updateSelectInput(session, "stage", choices = unique(filt_data$stage))
      updateSelectInput(session, "sample", choices = unique(filt_data$sample))
    }
  })

  observeEvent(list(
    input$data_opt, input$ident, input$species, input$type,
    input$condition, input$stage, input$sample
  ), {
    filt_data <- filtered_data() %>%
      select(any_of(c(
        "id", "ident", "seurat_clusters", "species", "type",
        "condition", "stage", "sample", "day", "replicate",
        "location", "vivax_strain", "n_count_rna", "n_feature_rna"
      )))

    if (!is.null(input$ident) || !is.null(input$species) ||
      !is.null(input$type) || !is.null(input$condition) ||
      !is.null(input$stage) || !is.null(input$sample)) {
      filt_data <- filt_data %>%
        filter(
          if (!is.null(input$ident)) ident %in% input$ident else TRUE,
          if (!is.null(input$species)) species %in% input$species else TRUE,
          if (!is.null(input$type)) type %in% input$type else TRUE,
          if (!is.null(input$condition)) condition %in% input$condition else TRUE,
          if (!is.null(input$stage)) stage %in% input$stage else TRUE,
          if (!is.null(input$sample)) sample %in% input$sample else TRUE
        )
    }


    if (input$data_opt == "meta") {
      shinyjs::show("meta_section")
      shinyjs::hide("qual_section")
      shinyjs::hide("source_section")

      output$data_table <- renderDT(
        {
          DT::datatable(
            filt_data %>%
              rename_with(~ str_to_title(.),
                .cols = any_of(c(
                  "id", "ident", "seurat_clusters",
                  "species", "type", "condition", "stage",
                  "sample", "day", "replicate", "location",
                  "vivax_strain"
                ))
              ) %>%
              rename(nCount_RNA = n_count_rna, nFeature_RNA = n_feature_rna) %>%
              arrange(desc(nCount_RNA)),
            options = list(pageLength = 10, scrollX = TRUE),
            selection = "single",
            rownames = FALSE
          )
        },
        server = TRUE
      )

      ## stage vs count
      output$stage_plotly <- renderPlotly({
        meta_plotly(filt_data, "stage", "Count", "Stage", "bar")
      })

      ## sample vs count
      output$sample_plotly <- renderPlotly({
        meta_plotly(filt_data, "sample", "Count", "Sample", "bar")
      })

      ## strain vs count
      output$strain_plotly <- renderPlotly({
        meta_plotly(filt_data, "vivax_strain", "Count", "Strain", "bar")
      })

      ## species vs count
      output$species_plotly <- renderPlotly({
        meta_plotly(filt_data, "species", "Count", "Species", "bar")
      })

      ## type vs count
      output$type_plotly <- renderPlotly({
        meta_plotly(filt_data, "type", "Count", "Type", "bar")
      })

      ## type vs count
      output$location_plotly <- renderPlotly({
        meta_plotly(filt_data, "location", "Count", "Location", "bar")
      })
    } else if (input$data_opt == "qual") {
      shinyjs::show("qual_section")
      shinyjs::hide("meta_section")
      shinyjs::hide("source_section")

      output$data_count_plotly <- renderPlotly({
        rna_feature_plot(filtered_data(),
          x = "sample", y = "n_count_rna",
          color = "ident", y_title = "RNA count", title = "RNA Count"
        )
      })

      output$data_feature_plotly <- renderPlotly({
        rna_feature_plot(filtered_data(),
          x = "sample", y = "n_feature_rna",
          color = "ident", y_title = "nFeature count",
          title = "Feature Count"
        )
      })

      output$data_pca_stdv_plotly <- renderPlotly({
        elbow_plot(pca_std_df)
      })

      output$variable_genes_plotly <- renderPlotly({
        var_genes_plot(vf_data, top_n = 15)
      })
    } else {
      shinyjs::show("source_section")
      shinyjs::hide("qual_section")
      shinyjs::hide("meta_section")
      # further implementation with data sources
    }
  })

  # Gene Exp tabset
  autoUpdateExp <- reactiveVal(TRUE)
  observeEvent(list(input$check_exp_0, input$data_type_exp), {
    autoUpdateExp(FALSE)
    later(function() {
      autoUpdateExp(TRUE)
    }, 1)
  })

  choice_exp <- eventReactive(input$check_exp_0, {
    if (input$check_exp_0) {
      gene_data()$genes$gene
    } else {
      gene_data()$genes %>%
        filter(!str_detect(name, "^PVP01")) %>%
        pull(name)
    }
  })

  observeEvent(list(input$check_exp_0, input$data_type_exp), {
    req(gene_data())
    updateSelectizeInput(session, "genes_exp_0",
      label = if (isTRUE(input$check_exp_0)) "Gene ID" else "Gene name",
      choices = choice_exp(),
      server = TRUE
    )
  })

  df_exp <- reactive({
    req(autoUpdateExp(), gene_data(), input$genes_exp_0, input$group_exp)
    data <- gene_data()$data
    gene_exp <- if (!input$check_exp_0) {
      get_gene_id(input$genes_exp_0, gene_data()$genes)
    } else {
      input$genes_exp_0
    }

    if (!(gene_exp %in% colnames(data))) {
      data$gene_expr <- NA
    } else {
      data <- data %>% mutate(gene_expr = .data[[gene_exp]])
    }

    if (!(tolower(input$group_exp) %in% colnames(data))) {
      data[[input$group_exp]] <- NA
    }

    return(data)
  })

  output$gene_exp_plotly <- renderPlotly({
    req(df_exp())
    plot_gene_expression(df_exp(), tolower(input$group_exp))
  })

  ## gene expression link
  observeEvent(input$genes_exp_0, {
    req(input$genes_exp_0)
    gene_id <- get_gene_id(input$genes_exp_0, gene_data()$genes)
    update_gene_links(gene_id)
  })

  ## add button (click_add_btn from js)
  counter <- reactiveVal(0)
  observeEvent(input$click_add_btn, {
    btn_id <- input$click_add_btn
    id <- as.numeric(gsub("add_btn_?", "", btn_id))
    shinyjs::hide(paste0("add_btn_", id))
    df_exp_i <- generate_df_exp(id, input, gene_data, get_gene_id)

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
        plot_gene_expression(
          df_exp_i(),
          tolower(input[[paste0("group_exp_", id)]])
        )
      })
    } else {
      id <- id + 1
      newBlock(id, counter, choice_exp(), subBlock, session)
      output[[paste0("gene_exp_plotly_", id)]] <- renderPlotly({
        plot_gene_expression(
          df_exp_i(),
          tolower(input[[paste0("group_exp_", id)]])
        )
      })
    }
  })

  ## remove button (click_remove_btn from js)
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

  choice_exp1 <- eventReactive(input$click_toggle_exp, {
    is_checked <- input[[input$click_toggle_exp]]
    if (!is_checked) {
      gene_data()$genes$gene
    } else {
      gene_data()$genes %>%
        filter(!str_detect(name, "^PVP01")) %>%
        pull(name)
    }
  })

  observeEvent(list(input$click_toggle_exp, input$data_type_exp), {
    req(gene_data())
    req(input$click_toggle_exp)

    btn_id <- input$click_toggle_exp
    id <- as.numeric(gsub("check_exp_", "", btn_id))

    if (is.character(input$click_toggle_exp)) {
      is_checked <- input[[input$click_toggle_exp]]

      updateSelectizeInput(
        session,
        paste0("genes_exp_", id),
        label = if (isTRUE(is_checked)) "Gene name" else "Gene ID",
        choices = choice_exp1(),
        server = TRUE
      )
    }
  })

  observeEvent(input$change_gene_exp, {
    id <- input$change_gene_exp
    id <- as.numeric(gsub("genes_exp_", "", id))
    gene_id <- get_gene_id(input[[paste0("genes_exp_", id)]], gene_data()$genes)
    update_gene_links(gene_id)
  })

  # selectinput genes
  observeEvent(input$genes_n, {
    req(input$genes_n)
    if (input$color == "genes") {
      gene_id <- get_gene_id(input$genes_n, gene_data()$genes)
      update_gene_links(gene_id)
    }
  })

  choice <- eventReactive(list(input$check, input$data_type), {
    if (input$check) {
      gene_data()$genes$gene
    } else {
      gene_data()$genes %>%
        filter(!str_detect(name, "^PVP01")) %>%
        pull(name)
    }
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

  ## hide tab_swithch for tsne
  observeEvent(input$embedding, {
    if (input$embedding == "tsne") {
      runjs('$("#tab_switch").css("visibility", "hidden");')
    } else {
      runjs('$("#tab_switch").css("visibility", "visible");')
    }
  })

  ## get x- and y-axis selections for 2D plots
  observeEvent(list(
    input$embedding, input$plot, input$xaxis, input$check_all
  ), {
    if (input$plot == "2d") {
      shinyjs::show("axis")

      x_choices <- switch(input$embedding,
        "umap" = if (isTRUE(input$check_all)) umap_uint_columns else umap_columns,
        "pca" = if (isTRUE(input$check_all)) pc_columns else pc_int_columns,
        "tsne" = tsne_columns
      )

      updateSelectInput(session, "xaxis",
        choices = x_choices,
        selected = x_choices[1]
      )

      y_choices <- switch(input$embedding,
        "umap" = y_col(
          input$check_all, input$xaxis, umap_uint_columns,
          umap_columns
        ),
        "pca" = y_col(input$check_all, input$xaxis, pc_columns, pc_int_columns),
        "tsne" = setdiff(tsne_columns, input$xaxis)
      )

      updateSelectInput(session, "yaxis",
        choices = y_choices,
        selected = if (input$yaxis %in% y_choices) input$yaxis else y_choices[2]
      )
    } else {
      shinyjs::hide("axis")
    }
  })

  ## schedule update after 1 second
  observe({
    list(
      input$embedding, input$plot, input$xaxis, input$yaxis, input$color,
      input$size, input$genes_n, input$data_type, data(), input$check,
      input$check_all
    )
    autoUpdatePlot(FALSE)
    later(function() {
      autoUpdatePlot(TRUE)
    }, 1)
  })

  # update plots when toggles change
  observeEvent(list(input$check, input$check_all), {
    autoUpdatePlot(TRUE)
  })

  ## determine plot type
  get_plot_type <- reactive({
    switch(input$embedding,
      "pca" = if (isTRUE(input$check_all)) "PCA" else "PCA_integrated",
      "umap" = if (isTRUE(input$check_all)) "UMAP_unintegrated" else "UMAP",
      "tsne" = "tSNE"
    )
  })

  ## generate plotly plot
  output$all_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$genes),
        "cca_snn" = input$genes_n,
        input$color
      )
      data <- data()
      actual_plot_type <- get_plot_type()
      generate_plotly(
        data, input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size,
        plot_type = actual_plot_type, plot_dim = input$plot
      )
    })
  })

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

  # clear memory on session end (review scope as needed)
  onSessionEnded(function() {
    rm(list = ls(envir = sessionData), envir = sessionData)
    gc()
  })
}
