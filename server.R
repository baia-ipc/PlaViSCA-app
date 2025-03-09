library(shiny)
library(shinyjs)
library(plotly)
library(RColorBrewer)

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

  # Make 'data' a reactive object
  data <- reactive({
    df <- read.csv("pf-ch10x-data.csv")

    # Filter data based on input$stagelr
    filtered_df <- df[df$STAGE_LR %in% input$stagelr, ]

    # Columns to convert to factors
    cols_to_convert <- c("STAGE_LR", "STRAIN", "STAGE_HR", "STAGE_HR2")
    filtered_df[cols_to_convert] <- lapply(
      filtered_df[cols_to_convert],
      as.factor
    )

    return(filtered_df)
  })

  # Generate colors
  colors <- colorRampPalette(brewer.pal(n = 8, name = "Set2"))(50)

  # Handle 2d plots
  pc_columns <- c("PC_1", "PC_2", "PC_3")
  umap_columns <- c("UMAP_1", "UMAP_2", "UMAP_3")

  # Ensure data is available before creating selectInputs
  output$xaxis <- renderUI({
    if (input$tabset == "UMAP" && input$plot == "2d") {
      selectInput(
        width = "98%",
        "xaxis_input",
        "Select X-Axis",
        choices = umap_columns,
        selected = "UMAP_1"
      )
    } else if (input$tabset == "PCA" && input$plot == "2d") {
      selectInput(
        width = "98%",
        "xaxis_input",
        "Select X-Axis",
        choices = pc_columns,
        selected = "PC_1"
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
        selected = "UMAP_2"
      )
    } else if (input$tabset == "PCA" && input$plot == "2d") {
      req(input$xaxis_input)
      y_choices <- setdiff(pc_columns, input$xaxis_input)

      selectInput(
        width = "98%",
        "yaxis_input",
        "Select Y-Axis",
        choices = pc_columns,
        selected = "PC_2"
      )
    }
  })

  # UMAP plot
  output$umap_plotly <- renderPlotly({
    req(data())
    req(input$xaxis_input, input$yaxis_input, input$color)
    selected_color <- data()[[input$color]]
    req(input$xaxis_input %in% colnames(data()), input$yaxis_input %in% colnames(data()))

    if (input$plot == "2d") {
      # 2d plot for UMAP
      plot_ly(
        data = data(),
        x = data()[[input$xaxis_input]],
        y = data()[[input$yaxis_input]],
        type = "scatter",
        mode = "markers",
        color = ~selected_color,
        marker = list(size = input$size),
        colors = colors,
        width = 1200,
        height = 800
      ) %>%
        layout(
          xaxis = list(title = paste("X-", input$xaxis_input)),
          yaxis = list(title = paste("Y-", input$yaxis_input))
        ) %>%
        config(displaylogo = FALSE, scrollZoom = TRUE)
    } else {
      # 3d plot for UMAP
      plot_ly(
        data = data(),
        x = ~UMAP_1,
        y = ~UMAP_2,
        z = ~UMAP_3,
        type = "scatter3d",
        mode = "markers",
        color = ~selected_color,
        marker = list(size = input$size),
        colors = colors,
        width = 1200,
        height = 800
      ) %>%
        layout(
          scene = list(
            xaxis = list(title = "X-UMAP_1"),
            yaxis = list(title = "Y-UMAP_2"),
            zaxis = list(title = "Z-UMAP_3")
          )
        ) %>%
        config(displaylogo = FALSE, scrollZoom = TRUE)
    }
  })


  # PCA plot
  output$pca_plotly <- renderPlotly({
    req(data())
    req(input$xaxis_input, input$yaxis_input, input$color)
    # Dynamically select the color column
    selected_color <- data()[[input$color]]

    if (input$plot == "2d") {
      # 2d plot for pca
      plot_ly(
        data = data(),
        x = data()[[input$xaxis_input]],
        y = data()[[input$yaxis_input]],
        type = "scatter",
        mode = "markers",
        color = ~selected_color,
        marker = list(size = input$size),
        colors = colors,
        width = 1200,
        height = 800
      ) %>%
        layout(
          xaxis = list(title = paste("X-", input$xaxis_input)),
          yaxis = list(title = paste("Y-", input$yaxis_input))
        ) %>%
        config(displaylogo = FALSE, scrollZoom = TRUE)
    } else {
      # 3plot for PCA
      plot_ly(data(),
        x = ~PC_1,
        y = ~PC_2,
        z = ~PC_3,
        type = "scatter3d",
        mode = "markers",
        color = ~selected_color,
        marker = list(size = input$size),
        colors = colors,
        width = 1200,
        height = 800
      ) %>%
        layout(
          scene = list(
            xaxis = list(title = "X-PCA_1"),
            yaxis = list(title = "Y-PCA_2"),
            zaxis = list(title = "Z-PCA_3")
          )
        ) %>%
        config(displaylogo = FALSE, scrollZoom = TRUE)
    }
  })

  # handle download data
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
