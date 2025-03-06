library(shiny)
library(shinyjs)
library(plotly)
library(RColorBrewer)


ui <- fluidPage(
  useShinyjs(),

  # Link to CSS and JS files
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$script(src = "script.js"),

  # Menu Bar
  div(class = "menu-bar",
      div(class = "logo", img(src = "Pvivax.jpeg", height = "60px")),
      div(class = "menu-title",
          span("Plasmodium vivax mapping")),
       div(class = "menu-items",
          actionLink("home", label = tagList(icon("home"), " Home"), class = "menu-link"),
          span("|"),
          actionLink("data", label = tagList(icon("table"), " Data display"), class = "menu-link")
      )
  ),

  # Home Section
  div(id = "home_section",
      fluidRow(class = "introduction",
               p("Malaria caused by Plasmodium vivax remains a significant public health challenge in many parts of the world. While less lethal than Plasmodium falciparum, P. vivax infections are more widespread and persistent, causing recurrent relapses that complicate elimination efforts. Its ability to remain dormant in the liver for prolonged periods and resurface months or even years later makes it uniquely difficult to treat and eradicate. Despite its global burden, our understanding of P. vivax biology, epidemiology, and treatment lags behind, partly due to limited data sharing and collaboration across research communities."),
               p("One of the critical barriers in combating P. vivax malaria is the fragmented nature of data collection and knowledge sharing. Key datasets on genetic diversity, drug resistance, and disease patterns are often siloed within institutions, limiting comprehensive analyses and insights. This lack of coordination hampers progress in developing robust strategies to tackle P. vivax infections. Furthermore, regions most affected by P. vivax, such as South Asia, Southeast Asia, and South America, often have limited resources to gather and analyze the data necessary for actionable interventions. Bridging these gaps requires a collective effort to standardize, centralize, and share knowledge."),
               p("To address these challenges, we have developed a platform dedicated to facilitating the sharing of Plasmodium vivax-related data and knowledge. This innovative solution seeks to bridge gaps in global collaboration by integrating diverse datasets, promoting transparency, and offering advanced tools for data analysis and visualization. By empowering researchers and policymakers with centralized access to critical information, the platform fosters a culture of open data sharing. Our goal is to accelerate scientific discoveries, refine treatment protocols, and contribute meaningfully to the global fight against P. vivax malaria, paving the way for its eventual elimination.",
                 actionLink("explore_data", "Explore data >>")),

               div(class = "img", img(src = "pvivax.jpg"))
      )
  ),

  # Data Sets Section
  div(
    id = "data_sets_section",
    #style = "display:none;",

    fluidRow(
      column(width = 4, class = "sidebar",
             fluidRow(
                   checkboxGroupInput(
                     "stagelr",
                     "Stages filter:",
                     choices = list(
                       "Trophozoite" = "trophozoite",
                       "Ring" = "ring",
                       "Schizont" = "schizont",
                       "Gametocyte" = "gametocyte"
                     ),
                     selected = list("trophozoite", "ring", "schizont", "gametocyte")
                   )
             ),
             fluidRow(
               radioButtons(
                    "size",
                    "Size:",
                    choices = list(
                      "S" = 2,
                      "M" = 4,
                      "L" = 6
                    ),
                    selected = list("4")
                  )
             ),

             fluidRow(
               radioButtons(
                 "color",
                 "Color:",
                 choices = list(
                   "Stage" = "STAGE_HR",
                   "Stage 2" = "STAGE_HR2",
                   "Strain" = "STRAIN",
                   "Cluster" = "CLUSTER"

                 ),
                 selected = "STAGE_HR2"
               )
             ),

             fluidRow(
               radioButtons(
                 "plot",
                 "Plot",
                 choices = list(
                   "2D" = "2d",
                   "3D" = "3d"
                 ),
                 selected = "2d"
               )
             ),

             fluidRow(
               column(width = 6, 
                 uiOutput("xaxis")
               ),
               column(width = 6,
                 uiOutput("yaxis")
               )

             ),

             fluidRow(
               downloadButton("download_data","Download", class = "bt-download")
             )

             ),

      column(width = 8,
             tabsetPanel(id = "tabset",
               tabPanel(
                 "UMAP",
                 plotlyOutput("umap_plotly")
               ),

               tabPanel(
                 "PCA",
                 plotlyOutput("pca_plotly")
               )
             )

             )
    )

  )
  )


# server path
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
    filtered_df[cols_to_convert] <- lapply(filtered_df[cols_to_convert], as.factor)

    return(filtered_df)
  })

  # Generate colors
  colors <- colorRampPalette(brewer.pal(n = 8, name = "Set2"))(20)


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
    } else if (input$tabset == "PCA" && input$plot =="2d") {
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
    } else if (input$tabset == "PCA" && input$plot =="2d") {

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

    # Dynamically select the color column
    selected_color <- data()[[input$color]]

    # Check if the selected columns exist in the data
    req(input$xaxis_input %in% colnames(data()), input$yaxis_input %in% colnames(data()))

    if (input$plot == "2d") {
      # 2d plot for UMAP
      plot_ly(
        data = data(),
        x =  data()[[input$xaxis_input]],
        y =  data()[[input$yaxis_input]],
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
        color = ~ selected_color,
        marker = list(size = input$size),
        colors = colors,
        width = 1200,
        height = 800
      ) %>%
        layout(
          scene = list(
            xaxis = list(title = "X-PC_1"),
            yaxis = list(title = "Y-PC_2"),
            zaxis = list(title = "Z-PC_3")
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
      x =  data()[[input$xaxis_input]],
      y =  data()[[input$yaxis_input]],
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
          color = ~ selected_color,
          marker = list(size = input$size),
          colors = colors,
          width = 1200,
          height = 800
  ) %>%
    layout(
      scene = list(
        xaxis = list(title = "X-PC_1"),
        yaxis = list(title = "Y-PC_2"),
        zaxis = list(title = "Z-PC_3")
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

# call shinyApp
shinyApp(ui = ui, server = server)
