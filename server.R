# load library
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)

# source function
source("functions/functions.R")

## read data
list_data <- readRDS("data/cleaned_dataset.rds")
mr_data <- list_data$mr_data
vf_data <- list_data$vf_data
top_genes_exp <- list_data$top_genes_exp

# server function
server <- function(input, output, session) {
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

  observe(
    if (input$tabset == "DATA") {
      shinyjs::removeClass("sidebar", "visible")
    } else {
      shinyjs::addClass("sidebar", "visible")
    }
  )


  ### select column for 2d plots
  pc_columns <- grep("^pc_\\d+", colnames(mr_data),
    ignore.case = TRUE, value = TRUE
  )
  pc_int_columns <- grep("^int.*pca.*", colnames(mr_data),
    ignore.case = TRUE, value = TRUE
  )
  umap_columns <- grep("^umap_\\d+$", colnames(mr_data),
    ignore.case = TRUE, value = TRUE
  )
  umap_uint_columns <- grep("^umapunint.*", colnames(mr_data),
    ignore.case = TRUE, value = TRUE
  )
  tsne_columns <- grep("t_sne", colnames(mr_data),
    ignore.case = TRUE,
    value = TRUE
  )
  gene_names <- colnames(mr_data)[tail(seq_along(mr_data), 10)]

  ### filter data
  filtered_data <- reactiveVal(NULL)
  observeEvent(input$stagei, {
    filtered_data(mr_data %>% filter(stage %in% input$stagei))
  })
  
  
  ### DATA tab
  observeEvent(input$tabset, {
    req(input$tabset) 
    if (input$tabset == "DATA") {
      filtered_data <- filtered_data()
      filtered_data <- filtered_data %>%
        select(
          id, species, day, condition, stage, n_count_rna, n_feature_rna,
          sample, ident, umap_1, umap_2, pc_1, pc_2, t_sne_1,
          t_sne_2
        )

      #### DT table
      output$data_table <- renderDT({
        DT::datatable(filtered_data,
          options = list(pageLength = 10, scrollX = TRUE),
          selection = "single",
          rownames = FALSE
        )
      }, server = TRUE)

      #### violin plot
      output$data_count_plotly <- renderPlotly({
        violin_plotly(filtered_data,
          x = "sample", y = "n_count_rna",
          color = "ident"
        )
      })

      output$data_feature_plotly <- renderPlotly({
        violin_plotly(filtered_data,
          x = "sample", y = "n_feature_rna",
          color = "ident"
        )
      })

      #### variable genes plot
      output$variable_genes_plotly <- renderPlotly({
        var_genes_plot(vf_data, top_n = 15)
      })

      ### top genes expression plot
      output$top_genes_exp_plotly <- renderPlotly({
        top_genes_exp(top_genes_exp)
      })
    }
  })

  # dynamic ui for gene name
  observeEvent(input$color, {
    if (input$color == "genes") {
      output$genes_ui <- renderUI({
        selectInput("genes_n", "Select Gene", choices = gene_names)
      })
    } else {
      output$genes_ui <- renderUI({
        NULL
      })
    }
  })


  ## define x and y axis
  output$xaxis <- renderUI({
    if (input$tabset == "UMAP" && input$plot == "2d") {
      selectInput(
        "xaxis_input",
        "Select X-Axis",
        choices = umap_columns,
        selected = "umap_1"
      )
    } else if (input$tabset == "UMAP_unintegred" && input$plot == "2d") {
      selectInput(
        "xaxis_input",
        "Select X-Axis",
        choices = umap_uint_columns,
        selected = "umapunintegrated_1"
      )
    } else if (input$tabset == "PCA" && input$plot == "2d") {
      selectInput(
        "xaxis_input",
        "Select X-Axis",
        choices = pc_columns,
        selected = "pc_1"
      )
    } else if (input$tabset == "PCA_integrated" && input$plot == "2d") {
      selectInput(
        "xaxis_input",
        "Select X-Axis",
        choices = pc_int_columns,
        selected = "integratedrpca_1"
      )
    } else if (input$tabset == "tSNE" && input$plot == "2d") {
      selectInput(
        "xaxis_input",
        "Select X-Axis",
        choices = tsne_columns,
        selected = "tsne_1"
      )
    }
  })

  output$yaxis <- renderUI({
    if (input$tabset == "UMAP" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(umap_columns, input$xaxis_input)

      selectInput(
        "yaxis_input",
        "Select Y-Axis",
        choices = y_choices,
        selected = "umap_2"
      )
    } else if (input$tabset == "UMAP_unintegred" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(umap_uint_columns, input$xaxis_input)

      selectInput(
        "yaxis_input",
        "Select Y-Axis",
        choices = umap_uint_columns,
        selected = "umapunintegrated_2"
      )
    } else if (input$tabset == "PCA" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(pc_columns, input$xaxis_input)

      selectInput(
        "yaxis_input",
        "Select Y-Axis",
        choices = pc_columns,
        selected = "pc_2"
      )
    } else if (input$tabset == "PCA_integrated" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(pc_columns, input$xaxis_input)

      selectInput(
        "yaxis_input",
        "Select Y-Axis",
        choices = pc_int_columns,
        selected = "integratedrpca_2"
      )
    } else if (input$tabset == "tSNE" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(tsne_columns, input$xaxis_input)

      selectInput(
        "yaxis_input",
        "Select Y-Axis",
        choices = y_choices,
        selected = "tsne_2"
      )
    }
  })


  ## plots
  output$pca_plotly <- renderPlotly({
    color_column <- if (input$color == "genes") input$genes_n else input$color
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "PCA", input$plot
    )
  })

  output$pca_int_plotly <- renderPlotly({
    color_column <- if (input$color == "genes") input$genes_n else input$color
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "PCA_integrated", input$plot
    )
  })

  output$umap_plotly <- renderPlotly({
    color_column <- if (input$color == "genes") input$genes_n else input$color
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "UMAP", input$plot
    )
  })

  output$umap_uint_plotly <- renderPlotly({
    color_column <- if (input$color == "genes") input$genes_n else input$color
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "UMAP_unintegred", input$plot
    )
  })

  output$tsne_plotly <- renderPlotly({
    color_column <- if (input$color == "genes") input$genes_n else input$color
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "tSNE", input$plot
    )
  })


  ## handle download data
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
}
