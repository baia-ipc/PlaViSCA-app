library(shiny)
library(shinyjs)
library(plotly)
library(shinycssloaders)

ui <- fluidPage(
  tags$head(
    useShinyjs(),
    includeCSS("www/style.css"),
    tags$script(src = "script.js"),
    tags$script(src = "jquery.rwdImageMaps.min.js"),
    tags$link(
      rel = "stylesheet",
      href = "https://code.jquery.com/ui/1.13.2/themes/smoothness/jquery-ui.css"
    ),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
            # Removed tip banner
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@900&display=swap",
      rel = "stylesheet"
    ),
    tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),
    tags$script(src = "particles.min.js"),
    tags$script(src = "particles-config.js")
  ),

  # Menubar
  div(
    class = "menu-bar",
    div(
      class = "logo ipc",
      a(
        href = "https://www.pasteur-kh.org/",
        target = "_blank",
        img(src = "ipc_logo.png", alt = "IPC Logo")
      )
    ),
    div(
      class = "menu-title",
      div(
        class = "title-text",
        h1("PlaViSca"),
        span(tags$i("Plasmodium vivax"), " Single Cell Analysis Platform")
      )
    ),
    div(
      class = "menu-items",
      actionLink(
        "home",
        label = tagList(icon("home"), " Home"),
        class = "menu-link active"
      ),
      span("|"),
      actionLink(
        "analysis",
        label = tagList(icon("table"), " Analysis"),
        class = "menu-link"
      ),
      span("|"),
      actionLink(
        "about",
        label = tagList(icon("info-circle"), " About"),
        class = "menu-link"
      ),
    ),
    div(class = "logo baia", img(src = "baia_logo.png"))
  ),

  # Home section
  div(
    id = "home_section",
    fluidRow(
      class = "introduction",
      column(
        width = 6,
        class = "intro-left",
        p(
          "PlaViSCA is a platform designed to support the malaria research
        community in analyzing and visualizing single-cell RNA sequencing
        (scRNA-seq) data. PlaViSca provides a user friendly interface for
        exploring metadata and gene expression across various parasite life
        cycle stages using interactive tables and 2D or 3D visualizations with
        customizable filters and coloring. The platform also allows users to
        download data for further analysis. PlaViSCA empowers researchers to
        explore complex scRNA-seq for ",
          em("P. vivax"),
          " without requiring any programming skills."
        ),
        div(
          class = "logo mosquito-bottom",
          img(
            src = "mosquito-bottom.png",
            alt = "mosquito"
          )
        )
      ),
      column(
        width = 6,
        class = "intro-right",
        div(
          class = "pages_link",
          actionLink(
            "metadata_link",
            tagList(
              img(src = "metadata.png", alt = "Metadata"),
              span("Metadata")
            )
          ),
          actionLink(
            "analysis_link",
            tagList(
              img(src = "umap.png", alt = "Embedding"),
              span("Embedding")
            )
          ),
          actionLink(
            "qc_link",
            tagList(
              img(src = "qc.png", alt = "Quality control"),
              span("Quality control")
            )
          )
        ),
        div(
          class = "pages_link",
          actionLink(
            "sources_link",
            tagList(
              img(src = "data_sources.png", alt = "Data sources"),
              span("Data sources")
            )
          ),
          actionLink(
            "gene_exp_link",
            tagList(
              img(src = "gene_exp.png", alt = "Gene Expression"),
              span("Gene expression")
            )
          ),
          actionLink(
            "contribute_link",
            tagList(
              img(src = "contribute.png", alt = "Contribute"),
              span("Contribute")
            )
          )
        ),
        div(
          class = "pages_link",
          actionLink(
            "usermanual_link",
            tagList(
              img(src = "usermanual.png", alt = "User Manual"),
              span("User Manual")
            )
          ),
          tags$a(
            href = "https://github.com/Sopheap15/pvsca",
            target = "_blank",
            tagList(
              img(
                src = "sources_code.png",
                alt = "Source code",
                height = "20px"
              ),
              span("Source code")
            )
          ),
          actionLink(
            "about_link",
            tagList(
              img(src = "about.png", alt = "About"),
              span("About")
            )
              )
            )
         )
         ),
    fluidRow(id = "particles-js-home", class = "particles-js"),
    fluidRow(p(class = "copyright", "© 2025 BAIA Team. All rights reserved."))
  ),

  # Analysis section
  div(
    id = "analysis_section",
    fluidRow(
      class = "analysis-container",
      column(
        width = 4,
        id = "sidebar",
        class = "sidebar",
        style = "display: none;",  # Hide sidebar - controls moved to Embeddings tab
        column(
          width = 12,
          class = "col-12",
          selectInput(
            "color",
            "Color",
            choices = list(
              "Study" = "study_label",
              "Development phase" = "development_phase",
              "Blood stage" = "blood_stage",
              "Parasite stage" = "parasite_stage",
              "Strain" = "strain",
              "Treatment" = "treatment",
              "Sample type" = "sample_type",
              "Host species" = "host_species",
              "Sequencer" = "sequencer",
              "SC. technology" = "sc_technology",
              "DPI" = "day_post_infection",
              "HPI" = "hour_post_invasion",
              "Genes" = "genes"
            ),
             selected = "study_label"
          )
        ),
        # Gene selection for first plot when genes is selected
        conditionalPanel(
          condition = "input.color == 'genes'",
          column(
            width = 12,
            class = "col-12",
            # Gene selection with toggle in label
            fluidRow(
              column(
                width = 12,
                style = "margin-bottom: 5px;",
                div(
                  style = "display: flex; align-items: center; margin-bottom: 5px;",
                  tags$label(
                    style = "margin: 0; margin-right: 10px; font-weight: bold;",
                    id = "gene_label",
                    "Gene name"
                  ),
                  tags$label(
                    class = "switch",
                    style = "margin: 0;",
                    tags$input(
                      id = "check",
                      type = "checkbox",
                      class = "toggle_exp"
                    ),
                    tags$span(class = "slider")
                  )
                ),
                selectizeInput("genes_n", NULL, 
                             choices = "",
                             multiple = TRUE,
                             options = list(
                               placeholder = "Select one or more genes",
                               maxItems = 10
                             ))
              )
            ),
            # External links inline with dropdown
            fluidRow(
              column(
                width = 12,
                tags$div(
                  id = "gene_link_ui_first",
                  class = "gene_link_ui_exp",
                  style = "margin-top: -15px; margin-bottom: 10px; text-align: left; padding-left: 0px; display: flex; align-items: center;",
                  tags$a(
                    id = "plasmo_link",
                    href = "#",
                    target = "_blank",
                    class = "gene-link",
                    "🔬 PlasmoDB"
                  ),
                  tags$span(" | ", style = "margin: 0 8px; color: #999; font-size: 1em;"),
                  tags$a(
                    id = "ncbi_link", 
                    href = "#",
                    target = "_blank",
                    class = "gene-link",
                    "🧬 NCBI"
                  )
                )
              )
            )
          )
        ),
        column(
          width = 12,
          class = "col-12",
          style = "margin-top: 15px;",
          fluidRow(
            column(
              width = 12,
              radioButtons(
                "data_type",
                "Data type",
                choices = list(
                  "Raw" = "raw",
                  "Normalized" = "normalize"
                  # "Scaled" = "scale"
                ),
                selected = "normalize"
               )
             )
           )
         ),
        column(
          width = 12,
          class = "col-12",
          checkboxInput(
            "enable_comparison",
            "Compare Two Plots",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.enable_comparison",
            selectInput(
              "color_second",
              "Second Plot Color",
              choices = list(
                "Study" = "study_label",
                "Development phase" = "development_phase",
                "Blood stage" = "blood_stage",
                "Parasite stage" = "parasite_stage",
                "Strain" = "strain",
                "Treatment" = "treatment",
                "Sample type" = "sample_type",
                "Host species" = "host_species",
                "Sequencer" = "sequencer",
                "SC. technology" = "sc_technology",
                "DPI" = "day_post_infection",
                "HPI" = "hour_post_invasion",
                "Genes" = "genes"
              ),
              selected = "sample_type"
            ),
             # Gene selection for second plot when genes is selected
             conditionalPanel(
               condition = "input.color_second == 'genes'",
               # Gene selection with toggle in label
               fluidRow(
                 column(
                   width = 12,
                   style = "margin-bottom: 5px;",
                   div(
                     style = "display: flex; align-items: center; margin-bottom: 5px;",
                     tags$label(
                       style = "margin: 0; margin-right: 10px; font-weight: bold;",
                       id = "gene_label_second",
                       "Gene name"
                     ),
                     tags$label(
                       class = "switch",
                       style = "margin: 0;",
                       tags$input(
                         id = "check_second",
                         type = "checkbox",
                         class = "toggle_exp"
                       ),
                       tags$span(class = "slider")
                     )
                   ),
                   selectizeInput("genes_second", NULL, 
                                choices = "", 
                                multiple = TRUE,
                                options = list(
                                  placeholder = "Select one or more genes",
                                  maxItems = 10
                                ))
                 )
               ),
               # External links inline with dropdown
               fluidRow(
                 column(
                   width = 12,
                  tags$div(
                    id = "gene_link_ui_second",
                    class = "gene_link_ui_exp",
                    style = "margin-top: -15px; margin-bottom: 10px; text-align: left; padding-left: 0px; display: flex; align-items: center;",
                    tags$a(
                      id = "plasmo_link_second",
                      href = "#",
                      target = "_blank", 
                      class = "gene-link",
                      "🔬 PlasmoDB"
                    ),
                    tags$span(" | ", style = "margin: 0 8px; color: #999; font-size: 1em;"),
                    tags$a(
                      id = "ncbi_link_second",
                      href = "#",
                      target = "_blank",
                      class = "gene-link",
                      "🧬 NCBI"
                    )
                  )
                 )
                )
              )
           )
         ),
         column(
          width = 12,
          class = "col-12",
          fluidRow(
            id = "axis",
            column(
              width = 6,
              id = "xaxis-container",
              selectInput(
                "xaxis",
                "X-axis",
                choices = c("pc_1"),
                selected = "pc_1"
              )
            ),
            column(
              width = 6,
              id = "yaxis-container",
              selectInput(
                "yaxis",
                "Y-axis",
                choices = c("pc_2"),
                selected = "pc_2"
              )
            )
          )
        ),
        column(
          width = 12,
          class = "col-12",
          downloadButton("download_data", "Download", class = "bt-download")
        )
      ),
      column(
        width = 12,
        id = "main",
        class = "main",
        
        tabsetPanel(
          id = "tabset",

          # embedding section (moved to first position)
          tabPanel(
            "Embeddings",
            # Shared filter row at top of tab content
            fluidRow(
              id = "filter-row-embeddings",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("")
              )
            ),
            
            # First row: Data type, Integration, Embedding type
            fluidRow(
              style = "margin-top: 20px; margin-bottom: 20px;",
              column(
                width = 12,
                div(
                  class = "filters-container",
                  style = "border: 1px solid #ddd; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
                  fluidRow(
                    # Data type
                    column(
                      width = 4,
                      tags$label("Data type:", style = "font-weight: 600; margin-bottom: 5px; display: block;"),
                      radioButtons(
                        "data_type",
                        NULL,
                        choices = list("Raw" = "raw", "Normalized" = "normalize"),
                        selected = "normalize",
                        inline = FALSE
                      )
                    ),
                    
                    # Integration toggle
                    column(
                      width = 4,
                      tags$label("Integration:", style = "font-weight: 600; margin-bottom: 5px; display: block;"),
                      div(
                        id = "tab_switch",
                        style = "text-align: center; margin-top: 10px;",
                        tags$span(class = "left-label", "Unintegrated"),
                        tags$label(
                          class = "switch",
                          tags$input(
                            id = "check_all",
                            type = "checkbox",
                            checked = TRUE
                          ),
                          tags$span(class = "slider")
                        ),
                        tags$span(class = "right-label", "Integrated")
                      )
                    ),
                    
                    # Embedding type
                    column(
                      width = 4,
                      tags$label("Embedding Type:", style = "font-weight: 600; margin-bottom: 5px; display: block;"),
                      radioButtons(
                        "embedding",
                        NULL,
                        inline = TRUE,
                        choices = list(
                          "PCA" = "pca",
                          "UMAP" = "umap",
                          "tSNE" = "tsne"
                        ),
                        selected = "umap"
                      )
                    )
                  )
                )
              )
            ),
            
            # Second row: Color by, Point size, Dimension (with axis on the right)
            fluidRow(
              style = "margin-bottom: 20px;",
              column(
                width = 12,
                div(
                  class = "filters-container",
                  style = "border: 1px solid #ddd; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
                  fluidRow(
                    # Color by
                    column(
                      width = 3,
                      selectInput(
                        "color",
                        "Color by:",
                        choices = list(
                          "Study" = "study_label",
                          "Development phase" = "development_phase",
                          "Blood stage" = "blood_stage",
                          "Parasite stage" = "parasite_stage",
                          "Strain" = "strain",
                          "Treatment" = "treatment",
                          "Sample type" = "sample_type",
                          "Host species" = "host_species",
                          "Sequencer" = "sequencer",
                          "SC. technology" = "sc_technology",
                          "DPI" = "day_post_infection",
                          "HPI" = "hour_post_invasion",
                          "Genes" = "genes"
                        ),
                        selected = "study_label"
                      )
                    ),
                    
                    # Point size
                    column(
                      width = 3,
                      radioButtons(
                        "size",
                        "Point Size:",
                        choices = list("Small" = 2, "Medium" = 4, "Large" = 6),
                        selected = 4,
                        inline = FALSE
                      )
                    ),
                    
                    # Dimension
                    column(
                      width = 3,
                      radioButtons(
                        "plot",
                        "Dimension:",
                        choices = list("2D" = "2d", "3D" = "3d"),
                        selected = "3d",
                        inline = FALSE
                      )
                    ),
                    
                    # X/Y axis controls (only shown for 2D plots) - VERTICAL on the right
                    column(
                      width = 3,
                      conditionalPanel(
                        condition = "input.plot == '2d'",
                        selectInput(
                          "xaxis",
                          "X-axis:",
                          choices = c("pc_1"),
                          selected = "pc_1"
                        ),
                        selectInput(
                          "yaxis",
                          "Y-axis:",
                          choices = c("pc_2"),
                          selected = "pc_2"
                        )
                      )
                    )
                  )
                )
              )
            ),
            
            # Compare two plots and download button
            fluidRow(
              style = "margin-bottom: 20px;",
              column(
                width = 12,
                div(
                  style = "text-align: center;",
                  div(
                    style = "display: inline-block; border: 1px solid #ddd; border-radius: 8px; padding: 15px 30px; background-color: #f0f8ff; margin-right: 20px;",
                    checkboxInput(
                      "enable_comparison",
                      HTML("<strong style='font-size: 1.1em;'>Compare Two Plots</strong>"),
                      value = FALSE
                    )
                  ),
                  downloadButton("download_data", "Download Data", class = "btn-primary btn-lg", style = "font-size: 1.1em; padding: 10px 30px;")
                )
              )
            ),
            
            # Gene selection (shown when Color = Genes)
            conditionalPanel(
              condition = "input.color == 'genes'",
              fluidRow(
                column(
                  width = 12,
                  div(
                    style = "background-color: #fffbea; padding: 15px; border-radius: 8px; border: 1px solid #ffd700; margin-bottom: 15px;",
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 10px;",
                      tags$label(
                        style = "margin: 0; margin-right: 10px; font-weight: bold;",
                        "Gene name"
                      ),
                      tags$label(
                        class = "switch",
                        style = "margin: 0;",
                        tags$input(
                          id = "check",
                          type = "checkbox",
                          class = "toggle_exp"
                        ),
                        tags$span(class = "slider")
                      )
                    ),
                    selectizeInput("genes_n", NULL, 
                                 choices = "",
                                 multiple = TRUE,
                                 options = list(
                                   placeholder = "Select one or more genes",
                                   maxItems = 10
                                 )),
                    tags$div(
                      style = "margin-top: 10px;",
                      tags$a(
                        id = "plasmo_link",
                        href = "#",
                        target = "_blank",
                        class = "gene-link",
                        "🔬 PlasmoDB"
                      ),
                      tags$span(" | ", style = "margin: 0 8px; color: #999;"),
                      tags$a(
                        id = "ncbi_link", 
                        href = "#",
                        target = "_blank",
                        class = "gene-link",
                        "🧬 NCBI"
                      )
                    )
                  )
                )
              )
            ),
            
            # Single plot (when comparison is disabled)
            conditionalPanel(
              condition = "!input.enable_comparison",
              # Plot
              fluidRow(
                column(
                  width = 12,
                  withSpinner(
                    type = 6,
                    size = 0.5,
                    color = "#3498db",
                    plotlyOutput("all_plotly", height = "650px")
                  )
                )
              )
            ),
            
            # Dual plots (when comparison is enabled)
            conditionalPanel(
              condition = "input.enable_comparison",
              fluidRow(
                # First plot
                column(
                  width = 6,
                  # Color selection for first plot
                  div(
                    style = "background-color: #e3f2fd; padding: 15px; border-radius: 8px; border: 2px solid #007bff; margin-bottom: 15px;",
                    tags$h5("First Plot", style = "color: #007bff; margin: 0 0 10px 0; font-weight: bold;"),
                    selectInput(
                      "color",
                      "Color by:",
                      choices = list(
                        "Study" = "study_label",
                        "Development phase" = "development_phase",
                        "Blood stage" = "blood_stage",
                        "Parasite stage" = "parasite_stage",
                        "Strain" = "strain",
                        "Treatment" = "treatment",
                        "Sample type" = "sample_type",
                        "Host species" = "host_species",
                        "Sequencer" = "sequencer",
                        "SC. technology" = "sc_technology",
                        "DPI" = "day_post_infection",
                        "HPI" = "hour_post_invasion",
                        "Genes" = "genes"
                      ),
                      selected = "study_label"
                    )
                  ),
                  # Gene selection for first plot
                  conditionalPanel(
                    condition = "input.color == 'genes'",
                    div(
                      style = "background-color: #fffbea; padding: 15px; border-radius: 8px; border: 1px solid #ffd700; margin-bottom: 15px;",
                      div(
                        style = "display: flex; align-items: center; margin-bottom: 10px;",
                        tags$label(
                          style = "margin: 0; margin-right: 10px; font-weight: bold;",
                          "Gene name"
                        ),
                        tags$label(
                          class = "switch",
                          style = "margin: 0;",
                          tags$input(
                            id = "check",
                            type = "checkbox",
                            class = "toggle_exp"
                          ),
                          tags$span(class = "slider")
                        )
                      ),
                      selectizeInput("genes_n", NULL, 
                                   choices = "",
                                   multiple = TRUE,
                                   options = list(
                                     placeholder = "Select one or more genes",
                                     maxItems = 10
                                   )),
                      tags$div(
                        style = "margin-top: 10px;",
                        tags$a(
                          id = "plasmo_link",
                          href = "#",
                          target = "_blank",
                          class = "gene-link",
                          "🔬 PlasmoDB"
                        ),
                        tags$span(" | ", style = "margin: 0 8px; color: #999;"),
                        tags$a(
                          id = "ncbi_link", 
                          href = "#",
                          target = "_blank",
                          class = "gene-link",
                          "🧬 NCBI"
                        )
                      )
                    )
                  ),
                  # First plot
                  withSpinner(
                    type = 6,
                    size = 0.4,
                    color = "#007bff",
                    plotlyOutput("first_plotly", height = "550px")
                  )
                ),
                
                # Second plot
                column(
                  width = 6,
                  # Color selection for second plot
                  div(
                    style = "background-color: #e8f5e9; padding: 15px; border-radius: 8px; border: 2px solid #28a745; margin-bottom: 15px;",
                    tags$h5("Second Plot", style = "color: #28a745; margin: 0 0 10px 0; font-weight: bold;"),
                    selectInput(
                      "color_second",
                      "Color by:",
                      choices = list(
                        "Study" = "study_label",
                        "Development phase" = "development_phase",
                        "Blood stage" = "blood_stage",
                        "Parasite stage" = "parasite_stage",
                        "Strain" = "strain",
                        "Treatment" = "treatment",
                        "Sample type" = "sample_type",
                        "Host species" = "host_species",
                        "Sequencer" = "sequencer",
                        "SC. technology" = "sc_technology",
                        "DPI" = "day_post_infection",
                        "HPI" = "hour_post_invasion",
                        "Genes" = "genes"
                      ),
                      selected = "sample_type"
                    )
                  ),
                  # Gene selection for second plot
                  conditionalPanel(
                    condition = "input.color_second == 'genes'",
                    div(
                      style = "background-color: #fffbea; padding: 15px; border-radius: 8px; border: 1px solid #ffd700; margin-bottom: 15px;",
                      div(
                        style = "display: flex; align-items: center; margin-bottom: 10px;",
                        tags$label(
                          style = "margin: 0; margin-right: 10px; font-weight: bold;",
                          "Gene name"
                        ),
                        tags$label(
                          class = "switch",
                          style = "margin: 0;",
                          tags$input(
                            id = "check_second",
                            type = "checkbox",
                            class = "toggle_exp"
                          ),
                          tags$span(class = "slider")
                        )
                      ),
                      selectizeInput("genes_second", NULL, 
                                   choices = "", 
                                   multiple = TRUE,
                                   options = list(
                                     placeholder = "Select one or more genes",
                                     maxItems = 10
                                   )),
                      tags$div(
                        style = "margin-top: 10px;",
                        tags$a(
                          id = "plasmo_link_second",
                          href = "#",
                          target = "_blank", 
                          class = "gene-link",
                          "🔬 PlasmoDB"
                        ),
                        tags$span(" | ", style = "margin: 0 8px; color: #999;"),
                        tags$a(
                          id = "ncbi_link_second",
                          href = "#",
                          target = "_blank",
                          class = "gene-link",
                          "🧬 NCBI"
                        )
                      )
                    )
                  ),
                  # Second plot
                  withSpinner(
                    type = 6,
                    size = 0.4,
                    color = "#28a745",
                    plotlyOutput("second_plotly", height = "550px")
                  )
                )
              )
            )
          ),

          # gene expression section (moved to second position)
          tabPanel(
            "Gene expression",
            # Shared filter row at top of tab content
            fluidRow(
              id = "filter-row-gene-expression",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("_gex")
              )
            ),
            # Data type, size and dimension box
            create_plot_controls("data_type_exp", TRUE, "_exp"),
            fluidRow(
              class = "gene-exp-row",
              div(
                id = "gene_exp_container",
                class = "gene-exp-row",
                column(
                  id = "gene_exp_tar",
                  width = 6,
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput("genes_exp_0", "Gene", 
                                   choices = NULL, 
                                   multiple = TRUE,
                                   options = list(
                                     placeholder = "Select one or more genes",
                                     maxItems = 10
                                   )),
                      column(
                        width = 2,
                        class = "switch_exp",
                        tags$label(
                          class = "switch",
                          tags$input(
                            id = "check_exp_0",
                            type = "checkbox",
                            class = "toggle_exp"
                          ),
                          tags$span(class = "slider")
          )
        ),
                      column(
                        width = 4,
                        id = "gene_link_exp",
                        class = "gene_link_exp",
                        tags$div(
                          id = "gene_link_ui_exp",
                          class = "gene_link_ui_exp",
                          tags$a(
                            id = "plasmo_link_exp",
                            href = "#",
                            target = "_blank",
                            class = "gene-link",
                            "🔬 PlasmoDB"
                          ),
                          tags$span(" | ", style = "margin: 0 8px; color: #999; font-size: 1em;"),
                          tags$a(
                            id = "ncbi_link_exp",
                            href = "#",
                            target = "_blank",
                            class = "gene-link",
                            "🧬 NCBI"
                          )
                        )
              )
            ),
                    column(
                      width = 6,
                      selectInput(
                        "group_exp",
                        "Group by",
                        choices = group_choices,
                        selected = "development_phase"
                      )
                    )
                  ),
                  fluidRow(
                    width = 12,
                    withSpinner(
                      type = 6,
                      size = 0.5,
                      color = "#3498db",
                      plotlyOutput("gene_exp_plotly")
                    ),
                    fluidRow(
                      class = "btn-add-rm-container",
                      downloadButton(
                        "download_gene_exp_0",
                        label = "",
                        class = "btn btn-secondary btn_add"
                      ),
                      actionButton(
                        "add_btn_0",
                        label = icon("plus"),
                        class = "btn btn-success btn_add"
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "Quality control",
            # Shared filter row at top of tab content
            fluidRow(
              id = "filter-row-quality-control",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("_qc")
              )
            ),
            # Data type, size and dimension box
            create_plot_controls("data_type_qc", TRUE, "_qc"),
            div(
              id = "qual_section",
              fluidRow(
                column(
                  width = 6,
                  withSpinner(
                    type = 6,
                    size = 0.5,
                    color = "#3498db",
                    plotlyOutput("data_count_plotly")
                  )
                ),
                column(
                  width = 6,
                  withSpinner(
                    type = 6,
                    size = 0.5,
                    color = "#3498db",
                    plotlyOutput("data_feature_plotly")
                  )
                )
              ),
              br(),
              fluidRow(
                column(width = 6, plotlyOutput("data_pca_stdv_plotly")),
                column(width = 6, plotlyOutput("variable_genes_plotly"))
              )
            )
          ),

          # Metadata tab (moved to fourth position)
          tabPanel(
            "Metadata",
            # Shared filter row at top of tab content
            fluidRow(
              id = "filter-row-metadata",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("_meta")
              )
            ),
            # Data type, size and dimension box
            create_plot_controls("data_type_meta", TRUE, "_meta"),
            div(
              id = "meta_section",
              fluidRow(
                column(width = 4, plotlyOutput("sample_plotly")),
                column(width = 4, plotlyOutput("species_plotly")),
                column(width = 4, plotlyOutput("strain_plotly"))
              ),
              fluidRow(
                column(width = 4, plotlyOutput("treatment_plotly")),
                column(
                  width = 4,
                  plotlyOutput("life_cycle_plotly"),
                  div(
                    style = "text-align: center; margin-top: 5px; font-size: 12px; color: #666;",
                    HTML(
                      "<em>Note: All parasite stages are predicted by SingleR, except sporozoites which are confirmed by sample type.</em>"
                    )
                  )
                ),
                column(width = 4, plotlyOutput("HPI_plotly"))
              ),
              br(),
              fluidRow(withSpinner(
                type = 6,
                size = 0.5,
                color = "#3498db",
                DT::DTOutput("data_table")
              ))
            )
          ),
          tabPanel(
            "Data sources",
            div(
              id = "source_section",
              column(
                width = 12,
                DT::DTOutput("data_source")
              )
            )
          )
        )
      )
    )
  ),

  # About section
  div(
    id = "about_section",
    fluidRow(
      class = "about",
      column(
        width = 8,
        p(
          "PlaViSca was developed by the Bioinformatics and Artificial Intelligence
        Applications (BAIA) Unit at the Institut Pasteur du Cambodge (IPC),
        in collaboration with the Malaria Unit at IPC and the Center for Tropical
        and Emerging Global Diseases at the University of Georgia, USA."
        ),
        p(
          "We are open to collaboration and welcome opportunities to integrate
        additional relevant data into this platform. Please reach out to us
        using following contact."
        )
      ),
      column(
        width = 4,
        id = "colab-particles",
        class = "collab-particles"
      )
    ),
    fluidRow(
      class = "contact",
      tags$ul(
        tags$li(
          strong("Sopheap Oeng"),
          " - Bioinformatic System Engineer, IPC, email: ",
          a(href = "mailto:osopheap@pasteur-kh.org", "osopheap@pasteur-kh.org")
        ),
        tags$li(
          strong("Anthony Ruberto"),
          " - Postdoctoral researcher at the Center
          for Tropical and Emerging Global Diseases, University of Georgia, USA,
          email: ",
          a(href = "mailto:aruberto@uga.edu", "aruberto@uga.edu")
        ),
        tags$li(
          strong("Thomas Obadia"),
          " - Bioinformatics and Biostatistcs Hub, Institut Pasteur, Paris, France.
  _         - G5 Infectious Diseases Epidemiology and Analytics, Institut Pasteur, Paris,
              France,
          email: ",
          a(href = "mailto:thomas.obadia@pasteur.fr", "thomas.obadia@pasteur.fr")
        ),
        tags$li(
          strong("Brice Feufack Donfack"),
          " - Postdoctoral researcher, Malaria Research Unit, IPC, email: ",
          a(
            href = "mailto:ldonfack@pasteur-kh.org",
            "ldonfack@pasteur-kh.org"
          )
        ),
        tags$li(
          strong("Giorgio Gonnella"),
          " - Head, BAIA Unit, IPC, email: ",
          a(
            href = "mailto:ggonnella@pasteur-kh.org",
            "ggonnella@pasteur-kh.org"
          )
        )
      ),
      p(
        "PlaViSca is built using the R Shiny framework. For more detail, please
        visit GitHub repo:",
        a(
          href = "https://github.com/Sopheap15/PlaViSca",
          HTML(
            'https://github.com/Sopheap15/PlaViSca
              <span class="point-emoji">👈 </span>'
          ),
          target = "_blank"
        ),
        "."
      )
    ),
    fluidRow(id = "particles-js-about", class = "particles-js"),
    fluidRow(p(class = "copyright", "© 2025 BAIA Team. All rights reserved."))
  )
)
