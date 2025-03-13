# load library
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)

# source function
source("functions/functions.R")

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
    }else {
      shinyjs::addClass("sidebar", "visible")
    }
  )
  
  ## read data
  list_data <- readRDS("data/cleaned_dataset.rds")
  mr_data <- list_data$mr_data
  normalized_genes_var <- list_data$normalized_genes_var
  top_genes_exp <- list_data$top_genes_exp
  
  ### select column for 2d plots
  pc_columns <- grep("pc", colnames(mr_data), ignore.case = TRUE, value = TRUE)
  umap_columns <- grep("um", colnames(mr_data), ignore.case = TRUE, value = TRUE)
  tsne_columns <- grep("t_sne", colnames(mr_data), ignore.case = TRUE, value = TRUE)
 
  ### filter data
  mr_flt_data <- reactive({
    mr_data <- mr_data %>% 
      filter(stage %in% input$stagei)
    return(mr_data)
  })
  
  ### DATA tab
  observeEvent(input$tabset, {
    if (input$tabset == "DATA") {
      mr_flt_data <- mr_flt_data()
      mr_flt_data <- mr_flt_data %>% 
        select(id, species, day, condition, stage, replicate, location,
               vivax_strain, n_count_rna, n_feature_rna, sample,
               seurat_clusters)
      
      #### DT table
     output$data_table <- renderDT({
       DT::datatable(mr_flt_data,
                     options = list(pageLength = 10),
                     selection = "single",
                     rownames = FALSE
                     )
     })
      
      #### violin plot
     output$data_count_plotly <- renderPlotly({
       violin_plotly(mr_flt_data, x = "sample", y = "n_count_rna",
                     color = "seurat_clusters")
     })
     
     output$data_feature_plotly <- renderPlotly({
       violin_plotly(mr_flt_data, x = "sample", y = "n_feature_rna",
                          color = "seurat_clusters")
     })
     
     #### variable genes plot
     output$variable_genes_plotly <- renderPlotly({
       var_genes_plot(normalized_genes_var)
     })
     
     ### top variable genes plot
     output$top_var_genes_plotly <- renderPlotly({
       top_genes_exp(top_genes_exp)
     })
     
    }
   })
  
  ## Ensure data is available before creating selectInputs
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


  ## plots
  output$pca_plotly <- renderPlotly({
    generate_plotly(mr_flt_data(), input$xaxis_input, input$yaxis_input,
                    input$color, input$size, "PCA", input$plot)
  })
  
  output$umap_plotly <- renderPlotly({
    generate_plotly(mr_flt_data(), input$xaxis_input, input$yaxis_input,
                    input$color, input$size, "UMAP", input$plot)
  })
  
  output$tsne_plotly <- renderPlotly({
    generate_plotly(mr_flt_data(), input$xaxis_input, input$yaxis_input,
                     input$color, input$size, "tSNE", input$plot)
  })
  

  ## handle download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Plasmodium_vivax_data", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.4, detail = "Saving file...")
        write.csv(mr_flt_data(), file, row.names = FALSE)
        incProgress(1, detail = "Download complete")
      })
    }
  )
}
