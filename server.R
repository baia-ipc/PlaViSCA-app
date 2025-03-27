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

# server function
server <- function(input, output, session) {
  ## flag update plots when it is TRUE
  autoUpdatePlot <- reactiveVal(FALSE)

  ## hide and show sections
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

  observe({
    if (input$tabset == "DATA") {
      shinyjs::removeClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-8").addClass("col-sm-11");')
    } else {
      shinyjs::addClass("sidebar", "visible")
      shinyjs::runjs('$("#main").removeClass("col-sm-11").addClass("col-sm-8");')
    }
  })


  # Toggle track visibility
  metadata_visible <- reactiveVal(FALSE)
  observeEvent(input$toggle_metadata, {
    shinyjs::toggle("metadata_section")
    metadata_visible(!metadata_visible())
    if (metadata_visible()) {
      shinyjs::html("arrow_meta", "&#9662;")
    } else {
      shinyjs::html("arrow_meta", "&#9656;")
    }
  })


  # to delete
  observeEvent(input$check, {
    print(input$check)
  })


  ## select columns for 2D plots
  pc_columns <- grep("^pc_\\d+", colnames(mr_data),
    ignore.case = TRUE,
    value = TRUE
  )
  pc_int_columns <- grep("^int.*pca.*", colnames(mr_data),
    ignore.case = TRUE,
    value = TRUE
  )
  umap_columns <- grep("^umap_\\d+$", colnames(mr_data),
    ignore.case = TRUE,
    value = TRUE
  )
  umap_uint_columns <- grep("^umapunint.*", colnames(mr_data),
    ignore.case = TRUE, value = TRUE
  )
  tsne_columns <- grep("t_sne", colnames(mr_data),
    ignore.case = TRUE,
    value = TRUE
  )
  cca_snn <- grep("CCA_snn", colnames(mr_data),
    ignore.case = TRUE,
    value = TRUE
  )

  # gene data and name
  gene_data <- eventReactive(input$data_type, {
    if (length(input$stagei) == 0) {
      return(NULL)
    } else {
      dataset <- switch(input$data_type,
        "raw" = raw_df,
        "normalize" = norm_df,
        "scale" = scale_df
      )
      list(
        data = dataset,
        genes = genes(dataset, gff_data) # genes from function.R
      )
    }
  })

  gene_data <- eventReactive(list(input$stagei, input$data_type), {
    if (length(input$stagei) == 0) {
      return(NULL)
    } else {
      dataset <- switch(input$data_type,
        "raw" = raw_df,
        "normalize" = norm_df,
        "scale" = scale_df
      )
      list(
        data = dataset %>% filter(stage %in% input$stagei),
        genes = genes(dataset, gff_data)
      )
    }
  })

  # data from merge meta data
  filtered_data <- eventReactive(input$stagei,
    {
      if (length(input$stagei) == 0) {
        return(NULL)
      } else {
        return(mr_data %>% filter(stage %in% input$stagei))
      }
    },
    ignoreNULL = FALSE
  )


  data <- reactive({
    if (input$color == "genes") {
      return(gene_data()$data)
    } else {
      return(filtered_data())
    }
  })


  ## DATA tab
  observeEvent(input$tabset, {
    if (input$tabset == "DATA") {
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
    input$ident, input$species, input$type,
    input$condition, input$stage, input$sample
  ), {
    filt_data <- filtered_data()

    if (!is.null(input$ident) | !is.null(input$species) | !is.null(input$type) |
      !is.null(input$condition) | !is.null(input$stage) | !is.null(input$sample)) {
      filt_data <- filt_data %>%
        filter(
          if (!is.null(input$ident)) ident %in% input$ident else TRUE,
          if (!is.null(input$species)) species %in% input$species else TRUE,
          if (!is.null(input$type)) type %in% input$type else TRUE,
          if (!is.null(input$condition)) condition %in% input$condition else TRUE,
          if (!is.null(input$stage)) stage %in% input$stage else TRUE,
          if (!is.null(input$sample)) sample %in% input$sample else TRUE
        )
    } else {
      filt_data <- filtered_data()
    }

    output$data_table <- renderDT(
      {
        DT::datatable(filt_data,
          options = list(pageLength = 10, scrollX = TRUE),
          selection = "single",
          rownames = FALSE
        )
      },
      server = TRUE
    )


    # Violin plots
    output$data_count_plotly <- renderPlotly({
      rna_feature_plot(filt_data,
        x = "sample", y = "n_count_rna",
        color = "ident", y_title = "RNA count", title = "RNA Count"
      )
    })

    output$data_feature_plotly <- renderPlotly({
      rna_feature_plot(filt_data,
        x = "sample", y = "n_feature_rna",
        color = "ident", y_title = "nFeature count", title = "Feature Count"
      )
    })

    # PCA elbow plot
    output$data_pca_stdv_plotly <- renderPlotly({
      elbow_plot(pca_std_df)
    })

    # Variable genes plot
    output$variable_genes_plotly <- renderPlotly({
      var_genes_plot(vf_data, top_n = 15)
    })

    # Top genes expression plot
    output$top_genes_exp_plotly <- renderPlotly({
      top_genes_exp_plot(top_genes_exp)
    })
  })



  data_loaded <- reactiveVal(FALSE)
  observeEvent(input$tabset, {
    req(input$tabset)
    if (input$tabset == "DATA" && !data_loaded()) {
      filt_data <- filtered_data()
      filt_data <- filt_data %>%
        select(
          id, ident, species, type, day, condition, stage, sample, n_count_rna,
          n_feature_rna
        )

      # output$data_table <- renderDT(
      #   {
      #     DT::datatable(filt_data,
      #       options = list(pageLength = 10, scrollX = TRUE),
      #       selection = "single",
      #       rownames = FALSE
      #     )
      #   },
      #   server = TRUE
      # )

      # # Violin plots
      # output$data_count_plotly <- renderPlotly({
      #   rna_feature_plot(filt_data,
      #     x = "sample", y = "n_count_rna",
      #     color = "ident", y_title = "RNA count", title = "RNA Count"
      #   )
      # })
      #
      # output$data_feature_plotly <- renderPlotly({
      #   rna_feature_plot(filt_data,
      #     x = "sample", y = "n_feature_rna",
      #     color = "ident", y_title = "nFeature count", title = "Feature Count"
      #   )
      # })
      #
      # # PCA elbow plot
      # output$data_pca_stdv_plotly <- renderPlotly({
      #   elbow_plot(pca_std_df)
      # })
      #
      # # Variable genes plot
      # output$variable_genes_plotly <- renderPlotly({
      #   var_genes_plot(vf_data, top_n = 15)
      # })
      #
      # # Top genes expression plot
      # output$top_genes_exp_plotly <- renderPlotly({
      #   top_genes_exp_plot(top_genes_exp)
      # })

      data_loaded(TRUE)
    }
  })

  ## Dynamic UI for gene and cca_snn selection
  shinyjs::hide("gene_snn_ui")
  shinyjs::hide("data_type")
  observe({
    if (input$color == "genes") {
      shinyjs::show("data_type")
      shinyjs::show("gene_snn_ui")
      shinyjs::show("gene_link_ui")
      updateSelectizeInput(session, "genes_n",
        label = "Select Gene id",
        choices = gene_data()$genes$name,
        server = TRUE
      )
    } else if (input$color == "cca_snn") {
      shinyjs::show("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
      shinyjs::hide("data_type")
      updateSelectizeInput(session, "genes_n",
        label = "Select Resolution",
        choices = cca_snn,
        server = TRUE
      )
    } else {
      shinyjs::hide("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
      shinyjs::hide("data_type")
    }
  })

  observeEvent(input$genes_n, {
    req(input$genes_n)
    if (input$color == "genes") {
      gene_id <- get_gene_id(input$genes_n, gene_data()$genes)
      plasmo_url <- paste0(
        "https://plasmodb.org/plasmo/app/record/gene/",
        gene_id
      )
      ncbi_url <- paste0("https://www.ncbi.nlm.nih.gov/protein/", gene_id)
      shinyjs::runjs(sprintf("document.getElementById('plasmo_link').href =
                             '%s';", plasmo_url))
      shinyjs::runjs(sprintf("document.getElementById('ncbi_link').href =
                             '%s';", ncbi_url))
    }
  })

  ## observer for updating both x- and y-axis
  observeEvent(list(input$tabset, input$plot, input$xaxis), {
    if (input$plot == "2d") {
      shinyjs::show("axis")
      x_choices <- switch(input$tabset,
        "UMAP_integrated" = umap_columns,
        "UMAP_unintegrated" = umap_uint_columns,
        "PCA_unintergrated" = pc_columns,
        "PCA_integrated" = pc_int_columns,
        "tSNE_integrated" = tsne_columns
      )
      updateSelectInput(session, "xaxis",
        choices = x_choices,
        selected = x_choices[1]
      )

      y_choices <- switch(input$tabset,
        "UMAP_integrated" = setdiff(umap_columns, input$xaxis),
        "UMAP_unintegrated" = setdiff(umap_uint_columns, input$xaxis),
        "PCA_unintergrated" = setdiff(pc_columns, input$xaxis),
        "PCA_integrated" = setdiff(pc_int_columns, input$xaxis),
        "tSNE_integrated" = setdiff(tsne_columns, input$xaxis)
      )
      updateSelectInput(session, "yaxis",
        choices = y_choices,
        selected = if (input$yaxis %in% y_choices) input$yaxis else y_choices[2]
      )
    } else {
      shinyjs::hide("axis")
    }
  })

  # reset flag to FALSE and schedules it to become TRUE after 1 second
  observe({
    list(
      input$tabset, input$plot, input$xaxis, input$yaxis, input$color,
      input$size, input$genes_n, input$data_type, data()
    )
    autoUpdatePlot(FALSE)
    later(function() {
      autoUpdatePlot(TRUE)
    }, 1)
  })

  ## Plot rendering: wait until autoUpdatePlot is TRUE
  output$pca_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, autoUpdatePlot())

    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$gene),
        "cca_snn" = input$genes_n,
        input$color
      )

      print(color_column)

      generate_plotly(
        data(), input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size, "PCA", input$plot
      )
    })
  })


  output$pca_int_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, input$color, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$gene),
        "cca_snn" = input$genes_n,
        input$color
      )
      generate_plotly(
        data(), input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size, "PCA_integrated", input$plot
      )
    })
  })

  output$umap_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, input$color, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$gene),
        "cca_snn" = input$genes_n,
        input$color
      )
      generate_plotly(
        data(), input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size, "UMAP", input$plot
      )
    })
  })

  output$umap_uint_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, input$color, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$gene),
        "cca_snn" = input$genes_n,
        input$color
      )
      generate_plotly(
        data(), input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size, "UMAP_unintegred", input$plot
      )
    })
  })

  output$tsne_plotly <- renderPlotly({
    req(data(), input$xaxis, input$yaxis, input$color, autoUpdatePlot())
    isolate({
      color_column <- switch(input$color,
        "genes" = get_gene_id(input$genes_n, gene_data()$gene),
        "cca_snn" = input$genes_n,
        input$color
      )
      generate_plotly(
        data(), input$xaxis, input$yaxis,
        color_column, input$genes_n, input$size, "tSNE", input$plot
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

  ## clear memory
  onSessionEnded(function() {
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    gc()
    session$reload()
  })
}
