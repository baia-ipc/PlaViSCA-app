library(shiny)
library(shinyjs)
library(plotly)
library(RColorBrewer)

server <- function(input, output, session) {
  
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
  
  # Load and filter data
  data <- reactive({
    df <- read.csv("pf-ch10x-data.csv")
    filtered_df <- df[df$STAGE_LR %in% input$stagelr, ]
    cols_to_convert <- c("STAGE_LR", "STRAIN", "STAGE_HR", "STAGE_HR2")
    filtered_df[cols_to_convert] <- lapply(filtered_df[cols_to_convert], as.factor)
    return(filtered_df)
  })
  
  colors <- colorRampPalette(brewer.pal(n = 8, name = "Set2"))(20)
  pc_columns <- c("PC_1", "PC_2", "PC_3")
  umap_columns <- c("UMAP_1", "UMAP_2", "UMAP_3")
  
  output$xaxis <- renderUI({
    if (input$tabset == "UMAP" && input$plot == "2d") {
      selectInput("xaxis_input", "Select X-Axis", choices = umap_columns, selected = "UMAP_1", width = "98%")
    } else if (input$tabset == "PCA" && input$plot == "2d") {
      selectInput("xaxis_input", "Select X-Axis", choices = pc_columns, selected = "PC_1", width = "98%")
    }
  })
  
  output$yaxis <- renderUI({
    req(input$xaxis_input)
    y_choices <- setdiff(if (input$tabset == "UMAP") umap_columns else pc_columns, input$xaxis_input)
    selectInput("yaxis_input", "Select Y-Axis", choices = y_choices, selected = y_choices[1], width = "98%")
  })
  
  output$umap_plotly <- renderPlotly({
    req(data(), input$xaxis_input, input$yaxis_input, input$color)
    selected_color <- data()[[input$color]]
    
    plot_ly(data = data(),
            x = data()[[input$xaxis_input]],
            y = data()[[input$yaxis_input]],
            type = ifelse(input$plot == "2d", "scatter", "scatter3d"),
            mode = "markers",
            color = ~selected_color,
            marker = list(size = input$size),
            colors = colors,
            width = 1200,
            height = 800) %>%
      layout(
        xaxis = list(title = paste("X-", input$xaxis_input)),
        yaxis = list(title = paste("Y-", input$yaxis_input))
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  })
  
  output$pca_plotly <- renderPlotly({
    req(data(), input$xaxis_input, input$yaxis_input, input$color)
    selected_color <- data()[[input$color]]
    
    plot_ly(data = data(),
            x = data()[[input$xaxis_input]],
            y = data()[[input$yaxis_input]],
            type = ifelse(input$plot == "2d", "scatter", "scatter3d"),
            mode = "markers",
            color = ~selected_color,
            marker = list(size = input$size),
            colors = colors,
            width = 1200,
            height = 800) %>%
      layout(
        xaxis = list(title = paste("X-", input$xaxis_input)),
        yaxis = list(title = paste("Y-", input$yaxis_input))
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("P_vivax_data", Sys.Date(), ".csv")
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

