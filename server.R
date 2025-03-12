# load library
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)

# source function
source("functions.R")

server <- function(input, output, session) {
  # Show the Data Section when 'Explore data' is clicked
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

  # tabset data
  observe(
    if (input$tabset == "DATA"){
      shinyjs::removeClass("sidebar", "visible")
    }else {
      shinyjs::addClass("sidebar", "visible")
    }
  )
  
  # read data
  df <- readRDS("data/pvsca_data.rds")
  data <- reactive({
    filtered_df <- df[df$stage %in% input$location, ]
    return(filtered_df)
  })

  # select column 2d plots
  pc_columns <- grep("pc", colnames(df), ignore.case = TRUE, value = TRUE)
  umap_columns <- grep("um", colnames(df), ignore.case = TRUE, value = TRUE)
  tsne_columns <- grep("t_sne", colnames(df), ignore.case = TRUE, value = TRUE)
  cca_res_columns <- grep("CCA_snn", colnames(df), ignore.case = TRUE,
                          value = TRUE)

  # table for DATA tab
  observeEvent(input$tabset, {
    if (input$tabset == "DATA") {
      flt_data <- data() %>% 
        select(id, species, day, condition, stage, replicate, location,
               vivax_strain, n_count_rna, n_feature_rna, sample,
               seurat_clusters)
     output$data_table <- renderDT({
       DT::datatable(flt_data,
                     options = list(pageLength = 10),
                     selection = "single",
                     rownames = FALSE
                     )
     })
     
     output$data_count_plotly <- renderPlotly({
       violin_plotly(data = flt_data, x = "sample", y = "n_count_rna",
                     color = "seurat_clusters")
     })
     
     output$data_feature_plotly <- renderPlotly({
       violin_plotly(data = flt_data, x = "sample", y = "n_feature_rna",
                          color = "seurat_clusters")
     })
     
    }
   })
  
  # dynamic UI for CCA resolution
  observeEvent(input$color, {
    if (input$color == "cca_res") {
      output$cca_res_ui <- renderUI({
        selectInput("cca_res", "Choose CCA resolution:",
                    choices = cca_res_columns,
                    selected = "CCA_snn_res.0.1")
      })
    } else {
      output$cca_res_ui <- renderUI({NULL}) 
    }
  })
  

  # Ensure data is available before creating selectInputs
  output$xaxis <- renderUI({
    if (input$tabset == "UMAP" && input$plot == "2d") {
      selectInput(
        width = "98%",
        "xaxis_input",
        "Select X-Axis",
        choices = umap_columns,
        selected = "umap_1"
      )
    } else if (input$tabset == "PCA" && input$plot == "2d") {
      selectInput(
        width = "98%",
        "xaxis_input",
        "Select X-Axis",
        choices = pc_columns,
        selected = "pc_1"
      )
    } else if (input$tabset == "tSNE" && input$plot == "2d") {
      selectInput(
        width = "98%",
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
        width = "98%",
        "yaxis_input",
        "Select Y-Axis",
        choices = y_choices,
        selected = "umap_2"
      )
    } else if (input$tabset == "PCA" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(pc_columns, input$xaxis_input)

      selectInput(
        width = "98%",
        "yaxis_input",
        "Select Y-Axis",
        choices = pc_columns,
        selected = "pc_2"
      )
    } else if (input$tabset == "tSNE" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(tsne_columns, input$xaxis_input)

      selectInput(
        width = "98%",
        "yaxis_input",
        "Select Y-Axis",
        choices = y_choices,
        selected = "tsne_2"
      )
    }
  })


  # plots
  output$pca_plotly <- renderPlotly({
    color_column <- if (input$color == "cca_res") input$cca_res else input$color
    generate_plotly(data(), input$xaxis_input, input$yaxis_input,
                    color_column, input$size, "PCA", input$plot)
  })
  
  output$umap_plotly <- renderPlotly({
    color_column <- if (input$color == "cca_res") input$cca_res else input$color
    generate_plotly(data(), input$xaxis_input, input$yaxis_input,
                    color_column, input$size, "UMAP", input$plot)
  })
  
  output$tsne_plotly <- renderPlotly({
    color_column <- if (input$color == "cca_res") input$cca_res else input$color
    generate_plotly(data(), input$xaxis_input, input$yaxis_input,
                    color_column, input$size, "tSNE", input$plot)
  })
  

  # handle download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Plasmodium_vivax_data", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.4, detail = "Saving file...")
        write.csv(data(), file, row.names = FALSE)
        incProgress(1, detail = "Download complete")
      })
    }
  )
}
