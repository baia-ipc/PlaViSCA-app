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
      )
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
        width = 12,
        id = "main",
        class = "main",
        tabsetPanel(
          id = "tabset",

          # embedding section
          tabPanel(
            "Embeddings",
            # Shared filter row
            fluidRow(
              id = "filter-row-embeddings",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("")
              )
            ),

            # Control Row (Fixed duplication and alignment)
            fluidRow(
              style = "margin-top: 20px; margin-bottom: 20px;",
              column(
                width = 12,
                div(
                  class = "filters-container",
                  style = "border: 1px solid #ddd; border-radius: 8px; padding: 20px 20px 10px 20px; background-color: #f9f9f9; margin-bottom: 0;",
                  # Settings header
                  h4(
                    style = "margin-top: 0; margin-bottom: 0; color: #333; font-weight: 600; border-bottom: 2px solid #3498db; padding-bottom: 10px;",
                    icon("cog"),
                    " Plot settings"
                  ),
                  fluidRow(
                    style = "padding-top: 10px;",
                    # Data type
                    column(
                      width = 2,
                      radioButtons(
                        "data_type",
                        "Data Type",
                        choices = list("Raw" = "raw", "Normalized" = "normalize"),
                        selected = "normalize",
                        inline = TRUE
                      )
                    ),

                    # Integration toggle
                    column(
                      width = 3,
                      tags$label("Integration", class = "control-label", style = "margin-bottom: 5px; display: block;"),
                      div(
                        id = "tab_switch",
                        style = "text-align: center; margin-top: 15px; display: flex; align-items: center; gap: 10px;",
                        tags$span(class = "left-label", "Unintegrated", style = "font-size: 12px;"),
                        tags$label(
                          class = "switch",
                          style = "transform: scale(0.8); margin-bottom: 0;",
                          tags$input(
                            id = "check_all",
                            type = "checkbox",
                            checked = TRUE
                          ),
                          tags$span(class = "slider")
                        ),
                        tags$span(class = "right-label", "Integrated", style = "font-size: 12px;")
                      )
                    ),

                    # Embedding type
                    column(
                      width = 2,
                      radioButtons(
                        "embedding",
                        "Embedding",
                        inline = TRUE,
                        choices = list("PCA" = "pca", "UMAP" = "umap", "tSNE" = "tsne"),
                        selected = "umap"
                      )
                    ),

                    # Point size
                    column(
                      width = 2,
                      radioButtons(
                        "size",
                        "Point Size",
                        choices = list("S" = 2, "M" = 4, "L" = 6),
                        selected = 4,
                        inline = TRUE
                      )
                    ),

                    # Dimension & Axes
                    column(
                      width = 3,
                      tags$label("Dimension", class = "control-label", style = "margin-bottom: 5px; display: block;"),
                      div(
                        style = "display: flex; align-items: center; gap: 10px; height: 34px;",
                        radioButtons(
                          "plot",
                          NULL,
                          choices = list("2D" = "2d", "3D" = "3d"),
                          selected = "3d",
                          inline = TRUE
                        ),
                        conditionalPanel(
                          condition = "input.plot == '2d'",
                          div(
                            style = "display: inline-flex; gap: 8px; align-items: center;",
                            div(
                              style = "display: flex; flex-direction: column;",
                              tags$label("X", style = "margin-bottom: 0px; font-size: 10px; text-align: center;"),
                              selectInput("xaxis", NULL, choices = c("pc_1" = "pc_1"), selected = "pc_1", width = "90px")
                            ),
                            div(
                              style = "display: flex; flex-direction: column;",
                              tags$label("Y", style = "margin-bottom: 0px; font-size: 10px; text-align: center;"),
                              selectInput("yaxis", NULL, choices = c("pc_2" = "pc_2"), selected = "pc_2", width = "90px")
                            )
                          )
                        )
                      )
                    )
                  ),

                  # Comparison and Download (Formatted and Compact)
                  div(
                    style = "margin-top: 5px; border-top: 1px solid #eee; padding: 2px 0; display: flex; justify-content: center; align-items: center; gap: 40px; background-color: #fafafa; border-radius: 8px; margin-bottom: 0;",
                    div(
                      id = "compare_box",
                      checkboxInput(
                        "enable_comparison",
                        tagList(icon("table-columns"), HTML("&nbsp;&nbsp;"), "Compare Two Plots"),
                        value = FALSE
                      )
                    ),
                    div(
                      style = "display: flex; align-items: center;",
                      downloadButton("download_data", "Download Data", class = "btn-download-green", style = "min-width: 250px; border-radius: 20px; height: 44px; display: flex; align-items: center; justify-content: center; font-weight: 600; font-size: 1.1em; box-shadow: 0 2px 4px rgba(0,0,0,0.1);")
                    )
                  ),

                  # Color Selection(s) Row
                  fluidRow(
                    style = "margin-top: 10px;",
                    # Single Plot Color (visible when NOT comparing - Aligned Left)
                    conditionalPanel(
                      condition = "!input.enable_comparison",
                      column(
                        width = 6,
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
                          selected = "study_label",
                          width = "100%"
                        ),
                        conditionalPanel(
                          condition = "input.color == 'genes'",
                          div(
                            style = "background-color: #fffbea; padding: 10px 15px; border-radius: 8px; border: 1px solid #ffd700; margin-top: 5px; margin-bottom: 10px;",
                            fluidRow(
                              column(
                                width = 5,
                                div(
                                  style = "display: flex; align-items: center; gap: 10px; height: 34px;",
                                  tags$label(style = "margin: 0; font-weight: bold;", "Gene name"),
                                  tags$label(
                                    class = "switch",
                                    style = "margin: 0;",
                                    tags$input(id = "check", type = "checkbox", class = "toggle_exp"),
                                    tags$span(class = "slider")
                                  )
                                )
                              ),
                              column(
                                width = 7,
                                div(
                                  style = "display: flex; align-items: center; gap: 15px; height: 34px; justify-content: flex-end;",
                                  tags$a(id = "plasmo_link", href = "#", target = "_blank", class = "gene-link", "🔬 PlasmoDB"),
                                  tags$a(id = "ncbi_link", href = "#", target = "_blank", class = "gene-link", "🧬 NCBI")
                                )
                              )
                            ),
                            selectizeInput("genes_n", NULL, choices = "", multiple = TRUE, options = list(placeholder = "Select one or more genes", maxItems = 10), width = "100%")
                          )
                        )
                      )
                    ),
                    # Comparison Plot Colors (visible when comparing)
                    conditionalPanel(
                      condition = "input.enable_comparison",
                      column(
                        width = 6,
                        div(
                          style = "background-color: #e3f2fd; padding: 10px 15px; border-radius: 12px; border: 1px solid #2196f3; box-shadow: 0 4px 6px rgba(33, 150, 243, 0.1);",
                          tags$h5("First Plot Color By:", style = "color: #1976d2; margin-top: 0; margin-bottom: 10px; font-weight: bold;"),
                          fluidRow(
                            column(
                              width = 4,
                              selectInput(
                                "color_first_comp",
                                NULL,
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
                                selected = "study_label",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 8,
                              conditionalPanel(
                                condition = "input.color_first_comp == 'genes'",
                                div(
                                  style = "background-color: white; padding: 8px 12px; border-radius: 8px; border: 1px solid #bbdefb;",
                                  fluidRow(
                                    column(
                                      width = 6,
                                      div(
                                        style = "display: flex; align-items: center; gap: 8px; height: 24px;",
                                        tags$label(style = "margin: 0; font-weight: 600; font-size: 0.85em;", "Gene name"),
                                        tags$label(
                                          class = "switch",
                                          style = "transform: scale(0.7); margin: 0;",
                                          tags$input(id = "check_first_comp", type = "checkbox", class = "toggle_exp"),
                                          tags$span(class = "slider")
                                        )
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      div(
                                        style = "display: flex; align-items: center; gap: 10px; height: 24px; justify-content: flex-end; font-size: 0.8em;",
                                        tags$a(id = "plasmo_link_first", href = "#", target = "_blank", class = "gene-link", "🔬 PlasmoDB"),
                                        tags$a(id = "ncbi_link_first", href = "#", target = "_blank", class = "gene-link", "🧬 NCBI")
                                      )
                                    )
                                  ),
                                  selectizeInput("genes_n_first_comp", NULL, choices = "", multiple = TRUE, options = list(placeholder = "Select genes", maxItems = 10), width = "100%")
                                )
                              )
                            )
                          )
                        )
                      ),
                      column(
                        width = 6,
                        div(
                          style = "background-color: #f1f8e9; padding: 10px 15px; border-radius: 12px; border: 1px solid #4caf50; box-shadow: 0 4px 6px rgba(76, 175, 80, 0.1);",
                          tags$h5("Second Plot Color By:", style = "color: #388e3c; margin-top: 0; margin-bottom: 10px; font-weight: bold;"),
                          fluidRow(
                            column(
                              width = 4,
                              selectInput(
                                "color_second",
                                NULL,
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
                                selected = "sample_type",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 8,
                              conditionalPanel(
                                condition = "input.color_second == 'genes'",
                                div(
                                  style = "background-color: white; padding: 8px 12px; border-radius: 8px; border: 1px solid #dcedc8;",
                                  fluidRow(
                                    column(
                                      width = 6,
                                      div(
                                        style = "display: flex; align-items: center; gap: 8px; height: 24px;",
                                        tags$label(style = "margin: 0; font-weight: 600; font-size: 0.85em;", "Gene name"),
                                        tags$label(
                                          class = "switch",
                                          style = "transform: scale(0.7); margin: 0;",
                                          tags$input(id = "check_second", type = "checkbox", class = "toggle_exp"),
                                          tags$span(class = "slider")
                                        )
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      div(
                                        style = "display: flex; align-items: center; gap: 10px; height: 24px; justify-content: flex-end; font-size: 0.8em;",
                                        tags$a(id = "plasmo_link_second", href = "#", target = "_blank", class = "gene-link", "🔬 PlasmoDB"),
                                        tags$a(id = "ncbi_link_second", href = "#", target = "_blank", class = "gene-link", "🧬 NCBI")
                                      )
                                    )
                                  ),
                                  selectizeInput("genes_second", NULL, choices = "", multiple = TRUE, options = list(placeholder = "Select genes", maxItems = 10), width = "100%")
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),

            # REMOVED OLD GENE SELECTION BOX FROM HERE

            # Single plot
            conditionalPanel(
              condition = "!input.enable_comparison",
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

            # Comparison plots
            conditionalPanel(
              condition = "input.enable_comparison",
              fluidRow(
                column(
                  width = 6,
                  withSpinner(
                    type = 6,
                    size = 0.4,
                    color = "#007bff",
                    plotlyOutput("first_plotly", height = "550px")
                  )
                ),
                column(
                  width = 6,
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

          # Gene expression section
          tabPanel(
            "Gene expression",
            fluidRow(
              id = "filter-row-gene-expression",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("_gex")
              )
            ),
            create_plot_controls("data_type_exp", FALSE, "_exp"),
            fluidRow(
              id = "gene_exp_container",
              column(
                width = 6,
                id = "dynamic_col_0",
                subBlock(0)
              )
            )
          ),

          # Quality control section
          tabPanel(
            "Quality control",
            fluidRow(
              id = "filter-row-quality-control",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("_qc")
              )
            ),
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

          # Metadata tab
          tabPanel(
            "Metadata",
            fluidRow(
              id = "filter-row-metadata",
              class = "filter-row",
              column(
                width = 12,
                create_meta_filter_row("_meta")
              )
            ),
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

          # Data sources tab
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
          - G5 Infectious Diseases Epidemiology and Analytics, Institut Pasteur, Paris, France,
          email: ",
          a(href = "mailto:thomas.obadia@pasteur.fr", "thomas.obadia@pasteur.fr")
        ),
        tags$li(
          strong("Brice Feufack Donfack"),
          " - Postdoctoral researcher, Malaria Research Unit, IPC, email: ",
          a(href = "mailto:ldonfack@pasteur-kh.org", "ldonfack@pasteur-kh.org")
        ),
        tags$li(
          strong("Giorgio Gonnella"),
          " - Head, BAIA Unit, IPC, email: ",
          a(href = "mailto:ggonnella@pasteur-kh.org", "ggonnella@pasteur-kh.org")
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
