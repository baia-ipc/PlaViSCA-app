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

  # navigte home and data section
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

  # adjust ui for data and Gene Exp tabset
  observe({
    if (input$tabset == "Data" || input$tabset == "Gene Exp") {
      shinyjs::removeClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-8").addClass("col-sm-11");')
    } else {
      shinyjs::addClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-11").addClass("col-sm-8");')
    }
  })


  # select raw, normalized and scaled data and filter by stagei
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

  # Metadata filter by stagei
  filtered_data <- eventReactive(input$stagei,
    {
      req(input$stagei)
      mr_data %>% filter(stage %in% input$stagei)
    },
    ignoreNULL = FALSE
  )

  # dataset for further output based on color input
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

    if (!is.null(input$ident) || !is.null(input$species) || !is.null(input$type) ||
      !is.null(input$condition) || !is.null(input$stage) || !is.null(input$sample)) {
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
          # metadata table
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

      # stage vs count
      output$stage_plotly <- renderPlotly({
        meta_plotly(filt_data, "stage", "Count", "Stage", "bar")
      })

      # sample vs count
      output$sample_plotly <- renderPlotly({
        meta_plotly(filt_data, "sample", "Count", "Sample", "bar")
      })

      # strain vs count
      output$strain_plotly <- renderPlotly({
        meta_plotly(filt_data, "vivax_strain", "Count", "Strain", "bar")
      })

      # species vs count
      output$species_plotly <- renderPlotly({
        meta_plotly(filt_data, "species", "Count", "Species", "bar")
      })

      # type vs count
      output$type_plotly <- renderPlotly({
        meta_plotly(filt_data, "type", "Count", "Type", "bar")
      })

      # type vs count
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

      output$top_genes_exp_plotly <- renderPlotly({
        top_genes_exp_plot(top_genes_exp)
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
  observe({
    list(input$check_exp, input$data_type_exp)
    autoUpdateExp(FALSE)
    later(function() {
      autoUpdateExp(TRUE)
    }, 1)
  })

  choice_exp <- eventReactive(input$check_exp, {
    if (input$check_exp) {
      gene_data()$genes$gene
    } else {
      gene_data()$genes %>%
        filter(!str_detect(name, "^PVP01")) %>%
        pull(name)
    }
  })

  observeEvent(list(input$check_exp, input$data_type_exp), {
    req(gene_data())

    updateSelectizeInput(session, "genes_exp_0",
      label = if (input$check_exp) "Gene ID" else "Gene name",
      choices = choice_exp(),
      server = TRUE
    )
  })

  df_exp <- reactive({
    req(autoUpdateExp(), gene_data(), input$genes_exp_0, input$group_exp)

    data <- gene_data()$data
    gene_exp <- if (!input$check_exp) {
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

  # gene expression link
  observeEvent(input$genes_exp_0, {
    req(input$genes_exp_0)

    gene_id <- get_gene_id(input$genes_exp_0, gene_data()$genes)
    update_gene_links(gene_id)
  })

  # gene link
  observeEvent(input$genes_n, {
    req(input$genes_n)
    if (input$color == "genes") {
      gene_id <- get_gene_id(input$genes_n, gene_data()$genes)
      update_gene_links(gene_id)
    }
  })





  counter <- reactiveVal(0)
  # Function to handle adding new dynamic blocks
  addNewBlock <- function(new_count) {
    counter(new_count)
    insertUI(
      selector = "#gene_exp_container",
      where = "beforeEnd",
      ui = column(
        width = 6,
        id = paste0("dynamic_col_", new_count),
        dynamicBlock(new_count, choices = choice_exp())
      )
    )
  }

  # function to generate the data
  generate_df_exp <- function(new_count) {
    reactive({
      req(
        input[[paste0("genes_exp_", new_count)]],
        input[[paste0("group_exp_", new_count)]]
      )

      data <- gene_data()$data
      gene_exp <- if (!input[[paste0("check_exp_", new_count)]]) {
        get_gene_id(input[[paste0("genes_exp_", new_count)]], gene_data()$genes)
      } else {
        input[[paste0("genes_exp_", new_count)]]
      }

      if (!(gene_exp %in% colnames(data))) {
        data$gene_expr <- NA
      } else {
        data <- data %>% mutate(gene_expr = .data[[gene_exp]])
      }

      group_col <- input[[paste0("group_exp_", new_count)]]
      if (!(tolower(group_col) %in% colnames(data))) {
        data[[group_col]] <- NA
      }

      return(data)
    })
  }



  # Observe the initial "Add" button
  observeEvent(input$add_btn,
    {
      new_count <- counter() + 1
      counter(new_count)
      shinyjs::hide("add_btn")
      insertUI(
        selector = "#gene_exp_tar",
        where = "afterEnd",
        ui = column(
          width = 6,
          id = paste0("dynamic_col_", new_count),
          dynamicBlock(new_count, choices = choice_exp())
        )
      )
      df_exp_i <- generate_df_exp(new_count)

      observe({
        req(df_exp_i())

        output[[paste0("gene_exp_plotly_", new_count)]] <- renderPlotly({
          plot_gene_expression(
            df_exp_i(),
            tolower(input[[paste0("group_exp_", new_count)]])
          )
        })
      })
    },
    ignoreNULL = TRUE,
    once = TRUE
  )


  # Dynamically observe the Add button for the new block
  observeEvent(input[[paste0("add_btn_", counter())]],
    {
      print(counter())
      new_count <- counter() + 1
      counter(new_count)
      shinyjs::hide(paste0("add_btn_", new_count - 1))
      addNewBlock(new_count)

      df_exp_i <- generate_df_exp(new_count)

      observe({
        req(df_exp_i())

        output[[paste0("gene_exp_plotly_", new_count)]] <- renderPlotly({
          plot_gene_expression(
            df_exp_i(),
            tolower(input[[paste0("group_exp_", new_count)]])
          )
        })
      })
    },
    ignoreNULL = TRUE
  )


  observeEvent(input$click_remove_btn,
    {
      btn_id <- input$click_remove_btn
      block_id <- as.numeric(gsub("remove_btn_", "", btn_id))
      removeUI(selector = paste0("#dynamic_col_", block_id))
      if (block_id == 0) {
        shinyjs::show("add_btn")
      }
    },
    ignoreNULL = TRUE
  )







  # Show the main Add button if there are no blocks present
  observe({
    if (counter() == 1) {
      shinyjs::show("add_btn")
    }
  })

  observe({
    for (i in 1:counter()) {
      observeEvent(
        list(input[[paste0("check_exp_", i)]], input$data_type_exp),
        {
          is_checked <- input[[paste0("check_exp_", i)]]

          if (!is.null(is_checked) && is_checked) {
            updateSelectInput(session, paste0("genes_exp_", i),
              label = "Gene ID",
              choices = gene_data()$genes$gene
            )
          } else {
            updateSelectInput(session, paste0("genes_exp_", i),
              label = "Gene Name",
              choices = gene_data()$genes %>%
                filter(!str_detect(name, "^PVP01")) %>%
                pull(name)
            )
          }
        }
      )
    }
  })


  # get gene id or name for dynamic ui
  observe({
    for (i in 1:counter()) {
      observeEvent(input[[paste0("genes_exp_", i)]], {
        req(input[[paste0("genes_exp_", i)]])

        gene_id <- get_gene_id(input[[paste0("genes_exp_", i)]], gene_data()$genes)

        update_gene_links(gene_id)
      })
    }
  })


  # Switch gene id or name for pca, umap and tsne plot
  choice <- eventReactive(list(input$check, input$data_type), {
    if (input$check) {
      gene_data()$genes$gene
    } else {
      gene_data()$genes %>%
        filter(!str_detect(name, "^PVP01")) %>%
        pull(name)
    }
  })

  # Dynamic UI for gene and cca_snn selection
  shinyjs::hide("gene_snn_ui")
  shinyjs::hide("data_type")
  observe({
    if (input$color == "genes") {
      shinyjs::show("data_type")
      shinyjs::show("gene_snn_ui")
      shinyjs::show("gene_link_ui")
      shinyjs::show("switch_id")
      updateSelectizeInput(session, "genes_n",
        label = if (input$check) "Gene ID" else "Gene name",
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


  ## Observer for updating x- and y-axis selections for 2D plots
  observeEvent(list(
    input$tabset, input$plot, input$xaxis, input$check_pca,
    input$check_umap, input$check_tsne
  ), {
    if (input$plot == "2d") {
      shinyjs::show("axis")

      x_choices <- switch(input$tabset,
        "UMAP" = x_col(input$check_umap, umap_uint_columns, umap_columns),
        "PCA" = x_col(input$check_pca, pc_columns, pc_int_columns),
        "tSNE" = tsne_columns
      )

      updateSelectInput(session, "xaxis",
        choices = x_choices,
        selected = x_choices[1]
      )

      y_choices <- switch(input$tabset,
        "UMAP" = y_col(
          input$check_umap, input$xaxis, umap_uint_columns,
          umap_columns
        ),
        "PCA" = y_col(input$check_pca, input$xaxis, pc_columns, pc_int_columns),
        "tSNE" = setdiff(tsne_columns, input$xaxis)
      )

      updateSelectInput(session, "yaxis",
        choices = y_choices,
        selected = if (input$yaxis %in% y_choices) input$yaxis else y_choices[2]
      )
    } else {
      shinyjs::hide("axis")
    }
  })

  # Reset plot update flag and schedule update after 1 second
  observe({
    list(
      input$tabset, input$plot, input$xaxis, input$yaxis, input$color,
      input$size, input$genes_n, input$data_type, data(), input$check,
      input$check_pca, input$check_umap, input$check_tsne
    )
    autoUpdatePlot(FALSE)
    later(function() {
      autoUpdatePlot(TRUE)
    }, 1)
  })

  # Plots update when any of the toggles change
  observeEvent(list(
    input$check, input$check_pca, input$check_umap,
    input$check_tsne
  ), {
    autoUpdatePlot(TRUE)
  })

  ## Determine plot type based on toggle state before generating plots
  get_plot_type <- reactive({
    switch(input$tabset,
      "PCA" = if (isTRUE(input$check_pca)) "PCA_integrated" else "PCA",
      "UMAP" = if (isTRUE(input$check_umap)) "UMAP_unintegred" else "UMAP",
      "tSNE" = if (isTRUE(input$check_tsne)) "tSNE_integrated" else "tSNE"
    )
  })

  ## Plot pca, umap and tsne rendering
  output$pca_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$genes),
        "cca_snn" = input$genes_n,
        input$color
      )

      data <- data() %>%
        select(-any_of(c(umap_columns, umap_uint_columns, tsne_columns)))

      actual_plot_type <- get_plot_type()
      generate_plotly(
        data, input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size,
        plot_type = actual_plot_type, plot_dim = input$plot
      )
    })
  })

  output$umap_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$genes),
        "cca_snn" = input$genes_n,
        input$color
      )

      data <- data() %>%
        select(-any_of(c(pc_columns, pc_int_columns, tsne_columns)))

      actual_plot_type <- get_plot_type()
      generate_plotly(
        data, input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size,
        plot_type = actual_plot_type, plot_dim = input$plot
      )
    })
  })

  output$tsne_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$genes),
        "cca_snn" = input$genes_n,
        input$color
      )

      data <- data() %>%
        select(-any_of(c(
          umap_columns, umap_uint_columns, pc_columns,
          pc_int_columns
        )))

      actual_plot_type <- get_plot_type()

      generate_plotly(
        data, input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size,
        plot_type = actual_plot_type, plot_dim = input$plot
      )
    })
  })

  ## Handle download data
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

  ## Clear memory on session end (review scope as needed)
  onSessionEnded(function() {
    rm(list = ls(envir = sessionData), envir = sessionData)
    gc()
  })
}
