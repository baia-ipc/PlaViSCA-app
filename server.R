# load library
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(RColorBrewer)
library(tidyverse)

# source function
source("functions/functions.R")

# read data
list_data <- readRDS("data/cleaned_dataset.rds")
mr_data <- list_data$mr_data
pca_std_df <- list_data$pca_std_df
vf_data <- list_data$vf_data
top_genes_exp <- list_data$top_genes_exp

# read gff data
gff_data <- read.table("ref/PvivaxP01_gff_data.tsv", sep = "\t", header = TRUE)

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
  # gene_names <- colnames(mr_data)[tail(seq_along(mr_data), 100)]
  gene_names <- grep("pvp", colnames(mr_data), ignore.case = TRUE, value = TRUE) %>%
    as.data.frame() %>%
    setNames("gene") %>%
    left_join(gff_data, by = c("gene" = "id")) %>%
    mutate(name = coalesce(name, gene))

  cca_snn <- grep("CCA_snn", colnames(mr_data),
    ignore.case = TRUE,
    value = TRUE
  )

  ### filter data
  filtered_data <- reactiveVal(NULL)
  observeEvent(input$stagei, {
    filtered_data(mr_data %>% filter(stage %in% input$stagei))
  })


  ### DATA tab
  data_loaded <- reactiveVal(FALSE)
  observeEvent(input$tabset, {
    req(input$tabset)
    if (input$tabset == "DATA" && !data_loaded()) {
      filtered_data <- filtered_data()
      filtered_data <- filtered_data %>%
        select(
          id, species, day, condition, stage, n_count_rna, n_feature_rna,
          sample, ident, umap_1, umap_2, pc_1, pc_2, t_sne_1, t_sne_2
        )

      #### DT table
      output$data_table <- renderDT(
        {
          DT::datatable(filtered_data,
            options = list(pageLength = 10, scrollX = TRUE),
            selection = "single",
            rownames = FALSE
          )
        },
        server = TRUE
      )

      #### violin plot
      output$data_count_plotly <- renderPlotly({
        violin_plotly(filtered_data,
          x = "sample", y = "n_count_rna",
          color = "ident", title = "RNA Count"
        )
      })

      #### feature plot
      output$data_feature_plotly <- renderPlotly({
        violin_plotly(filtered_data,
          x = "sample", y = "n_feature_rna",
          color = "ident", title = "Feature Count"
        )
      })

      #### elbow plot
      output$data_pca_stdv_plotly <- renderPlotly({
        elbow_plot(pca_std_df)
      })


      #### variable genes plot
      output$variable_genes_plotly <- renderPlotly({
        var_genes_plot(vf_data, top_n = 15)
      })

      ### top genes expression plot
      output$top_genes_exp_plotly <- renderPlotly({
        top_genes_exp_plot(top_genes_exp)
      })

      data_loaded(TRUE)
    }
  })



  # dynamic ui for gene name
  shinyjs::hide("gene_snn_ui")
  observe(
    if (input$color == "genes") {
      shinyjs::show("gene_snn_ui")
      shinyjs::show("gene_link_ui")
      updateSelectizeInput(session, "genes_n",
        label = "Choose Gene",
        choices = gene_names$name
      )
    } else if (input$color == "cca_snn") {
      shinyjs::show("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
      updateSelectInput(session, "genes_n",
        label = "Select Resolution",
        choices = cca_snn
      )
    } else {
      shinyjs::hide("gene_snn_ui")
      shinyjs::hide("gene_link_ui")
    }
  )

  observeEvent(input$genes_n, {
    req(input$genes_n)
    if (input$color == "genes") {
      gene_id <- get_gene_id(input$genes_n, gene_names)
      plasmo_url <-
        paste0("https://plasmodb.org/plasmo/app/record/gene/", gene_id)
      ncbi_url <-
        paste0("https://www.ncbi.nlm.nih.gov/protein/", gene_id)

      shinyjs::runjs(sprintf("document.getElementById('plasmo_link').href
                             = '%s';", plasmo_url))
      shinyjs::runjs(sprintf("document.getElementById('ncbi_link').href
                             = '%s';", ncbi_url))
    }
  })


  ## define x and y axis
  # observe({
  # if (input$tabset == "UMAP" && input$plot == "2d") {
  #   shinyjs::show("xaxis")
  #   updateSelectInput(session, "xaxis", choices = umap_columns, selected = "umap_1")
  # }
  # })
  #
  # observe({
  #   if (input$plot == "3d") {
  #     shinyjs::hide("xaxis")
  #   }
  # })

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
    color_column <- switch(input$color,
      "genes" = get_gene_id(input$genes_n, gene_names),
      "cca_snn" = input$genes_n,
      input$color
    )
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "PCA", input$plot
    )
  })

  output$pca_int_plotly <- renderPlotly({
    color_column <- switch(input$color,
      "genes" = get_gene_id(input$genes_n, gene_names),
      "cca_snn" = input$genes_n,
      input$color
    )
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "PCA_integrated", input$plot
    )
  })

  output$umap_plotly <- renderPlotly({
    color_column <- switch(input$color,
      "genes" = get_gene_id(input$genes_n, gene_names),
      "cca_snn" = input$genes_n,
      input$color
    )
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "UMAP", input$plot
    )
  })

  output$umap_uint_plotly <- renderPlotly({
    color_column <- switch(input$color,
      "genes" = get_gene_id(input$genes_n, gene_names),
      "cca_snn" = input$genes_n,
      input$color
    )
    generate_plotly(
      filtered_data(), input$xaxis_input, input$yaxis_input,
      color_column, input$size, "UMAP_unintegred", input$plot
    )
  })

  output$tsne_plotly <- renderPlotly({
    color_column <- switch(input$color,
      "genes" = get_gene_id(input$genes_n, gene_names),
      "cca_snn" = input$genes_n,
      input$color
    )
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
