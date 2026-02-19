library(shiny)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(bslib)
library(fontawesome)

ui <- fluidPage(
  theme = bs_theme(version = 5),
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
      class = "menu-title",
      h1("PlaViSca"),
      span(tags$i("Plasmodium vivax"), " Single Cell Analysis Platform")
    ),
    div(
      class = "menu-items",
      actionLink(
        "home",
        label = tagList(icon("home"), " Home"),
        class = "menu-link active"
      ),
      actionLink(
        "analysis",
        label = tagList(icon("table"), " Analysis"),
        class = "menu-link"
      ),
      actionLink(
        "about",
        label = tagList(icon("info-circle"), " About"),
        class = "menu-link"
      )
    )
  ),
  
  # Home section
  div(
    id = "home_section",
    class = "home-section",
    div(id = "particles-js-home"),
    fluidRow(
      class = "introduction",
      column(
        width = 6,
        class = "intro-left",
        h2("Welcome to PlaViSca"),
        p(
          "An interactive platform for the analysis and visualization of single-cell RNA sequencing (scRNA-seq) data from ",
          em("P. vivax"),
          ". Explore metadata, gene expression, and various parasite life cycle stages through interactive tables and 2D/3D visualizations."
        )
      ),
      column(
        width = 6,
        class = "intro-right",
        div(
          class = "pages-links",
          actionLink(
            "metadata_link",
            tagList(
              img(src = "metadata.png", alt = "Metadata", class = "link-icon"),
              span("Metadata")
            )
          ),
          actionLink(
            "analysis_link",
            tagList(
              img(src = "umap.png", alt = "Embedding", class = "link-icon"),
              span("Embedding")
            )
          ),
          actionLink(
            "qc_link",
            tagList(
              img(src = "qc.png", alt = "Quality control", class = "link-icon"),
              span("Quality control")
            )
          ),
          actionLink(
            "sources_link",
            tagList(
              img(src = "data_sources.png", alt = "Data sources", class = "link-icon"),
              span("Data sources")
            )
          ),
          actionLink(
            "gene_exp_link",
            tagList(
              img(src = "gene_exp.png", alt = "Gene Expression", class = "link-icon"),
              span("Gene expression")
            )
          ),
          actionLink(
            "usermanual_link",
            tagList(
              img(src = "usermanual.png", alt = "User Manual", class = "link-icon"),
              span("User Manual")
            )
          )
        )
      )
    )
  ),
  
  # Analysis section
  div(
    id = "analysis_section",
    style = "display: none;",
    div(
      class = "analysis-dashboard",
      # Main Content for Analysis
      div(
        class = "analysis-content w-100",
        tabsetPanel(
          id = "tabset",
          
          # embedding section
          tabPanel(
            tagList(fontawesome::fa("diagram-project"), " Embeddings"),
            value = "Embeddings",
            # Shared filter row
            create_meta_filter_row(""),
            
            # Control Row (Fixed duplication and alignment)
            card(
              class = "mb-3 shadow-sm",
              card_header(
                class = "bg-secondary text-white py-2",
                tagList(fontawesome::fa("cog"), tags$strong(" Plot settings", class = "ms-2"))
              ),
              card_body(
                fluidRow(
                  class = "g-3 align-items-end",
                  column(
                    width = 2,
                    radioButtons(
                      "data_type",
                      tags$span("Data Type", style = "font-size: 0.85rem; font-weight: 500;"),
                      choices = list("Raw" = "raw", "Normalized" = "normalize"),
                      selected = "normalize",
                      inline = TRUE
                    )
                  ),
                  column(
                    width = 2,
                    conditionalPanel(
                      condition = "input.embedding != 'tsne'",
                      div(
                        id = "tab_switch",
                        class = "switch-container d-flex align-items-center gap-2 mt-4",
                        tags$span("Unintegrated", class = "small text-muted"),
                        input_switch(
                          "check_all",
                          label = NULL,
                          value = TRUE
                        ),
                        tags$span("Integrated", class = "small")
                      )
                    )
                  ),
                  column(
                    width = 3,
                    radioButtons(
                      "embedding",
                      tags$span("Embedding", style = "font-size: 0.85rem; font-weight: 500;"),
                      inline = TRUE,
                      choices = list("PCA" = "pca", "UMAP" = "umap", "tSNE" = "tsne"),
                      selected = "umap"
                    )
                  ),
                  column(
                    width = 2,
                    radioButtons(
                      "size",
                      tags$span("Point Size", style = "font-size: 0.85rem; font-weight: 500;"),
                      choices = list("S" = 2, "M" = 4, "L" = 6),
                      selected = 4,
                      inline = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    div(
                      class = "d-flex align-items-center gap-3",
                      radioButtons(
                        "plot",
                        tags$span("Dim", style = "font-size: 0.85rem; font-weight: 500;"),
                        choices = list("2D" = "2d", "3D" = "3d"),
                        selected = "3d",
                        inline = TRUE
                      ),
                      conditionalPanel(
                        condition = "input.plot == '2d'",
                        div(
                          class = "d-flex gap-1",
                          selectInput("xaxis", NULL, choices = c("pc_1" = "pc_1"), selected = "pc_1", width = "80px"),
                          selectInput("yaxis", NULL, choices = c("pc_2" = "pc_2"), selected = "pc_2", width = "80px")
                        )
                      )
                    )
                  )
                ),
                
                hr(class = "my-2"),
                
                div(
                  class = "d-flex justify-content-between align-items-center",
                  div(
                    class = "d-flex align-items-center gap-4",
                    checkboxInput(
                      "enable_comparison",
                      tagList(fontawesome::fa("columns"), HTML("&nbsp;"), "Compare mode"),
                      value = FALSE
                    ),
                    conditionalPanel(
                      condition = "!input.enable_comparison",
                      div(
                        class = "d-flex align-items-center gap-2",
                        tags$span("Color by:", style = "font-size: 0.85rem; font-weight: 500;"),
                        selectInput(
                          "color",
                          NULL,
                          choices = list(
                            "Study" = "study_label",
                            "Phase" = "development_phase",
                            "Parasite stages" = "parasite_stages",
                            "Strain" = "strain",
                            "Treatment" = "treatment",
                            "Sample type" = "sample_type",
                            "Host species" = "host_species",
                            "Genes" = "genes"
                          ),
                          selected = "development_phase",
                          width = "160px"
                        )
                      )
                    )
                  ),
                  downloadButton("download_data", "Data", class = "btn-sm btn-success")
                ),
                
                # Gene selection row (compact)
                conditionalPanel(
                  condition = "input.color == 'genes' && !input.enable_comparison",
                  div(
                    class = "mt-2 p-2 border rounded bg-light",
                    fluidRow(
                      column(
                        width = 4,
                        selectizeInput("genes_n", NULL, choices = "", multiple = TRUE, options = list(placeholder = "Search genes...", maxItems = 10), width = "100%")
                      ),
                      column(
                        width = 8,
                        div(
                          class = "d-flex align-items-center gap-3 mt-1",
                          input_switch("check", "Show Gene ID", value = FALSE),
                          tags$a(id = "plasmo_link", href = "#", target = "_blank", class = "btn btn-outline-info btn-sm", tagList(fontawesome::fa("external-link-alt"), " PlasmoDB")),
                          tags$a(id = "ncbi_link", href = "#", target = "_blank", class = "btn btn-outline-info btn-sm", tagList(fontawesome::fa("external-link-alt"), " NCBI"))
                        )
                      )
                    )
                  )
                ),

                # Comparison Plot Colors (visible when comparing)
                conditionalPanel(
                  condition = "input.enable_comparison",
                  div(
                    class = "mt-2 p-2 border rounded bg-light",
                    fluidRow(
                      column(
                        width = 6,
                        div(
                          class = "d-flex align-items-center gap-2",
                          tags$strong("Plot 1:", class = "small"),
                          selectInput(
                            "color_first_comp",
                            NULL,
                            choices = list(
                              "Study" = "study_label", "Phase" = "development_phase", "Parasite stages" = "parasite_stages",
                              "Strain" = "strain", "Treatment" = "treatment", "Sample type" = "sample_type",
                              "Host species" = "host_species", "Genes" = "genes"
                            ),
                            selected = "development_phase", width = "140px"
                          ),
                          conditionalPanel(
                            condition = "input.color_first_comp == 'genes'",
                            selectizeInput("genes_n_first_comp", NULL, choices = "", multiple = TRUE, options = list(placeholder = "Genes...", maxItems = 10), width = "200px")
                          )
                        )
                      ),
                      column(
                        width = 6,
                        div(
                          class = "d-flex align-items-center gap-2",
                          tags$strong("Plot 2:", class = "small"),
                          selectInput(
                            "color_second",
                            NULL,
                            choices = list(
                              "Study" = "study_label", "Phase" = "development_phase", "Parasite stages" = "parasite_stages",
                              "Strain" = "strain", "Treatment" = "treatment", "Sample type" = "sample_type",
                              "Host species" = "host_species", "Genes" = "genes"
                            ),
                            selected = "development_phase", width = "140px"
                          ),
                          conditionalPanel(
                            condition = "input.color_second == 'genes'",
                            selectizeInput("genes_second", NULL, choices = "", multiple = TRUE, options = list(placeholder = "Genes...", maxItems = 10), width = "200px")
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            
            # Single plot
            conditionalPanel(
              condition = "!input.enable_comparison",
              card(
                full_screen = TRUE,
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  "Visualization"
                ),
                withSpinner(
                  type = 6,
                  size = 0.5,
                  color = "#3498db",
                  plotlyOutput("all_plotly", height = "650px")
                )
              )
            ),
            
            # Comparison plots
            conditionalPanel(
              condition = "input.enable_comparison",
              layout_column_wrap(
                width = 1/2,
                card(
                  full_screen = TRUE,
                  card_header("First Plot"),
                  withSpinner(
                    type = 6,
                    size = 0.4,
                    color = "#007bff",
                    plotlyOutput("first_plotly", height = "550px")
                  )
                ),
                card(
                  full_screen = TRUE,
                  card_header("Second Plot"),
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
            tagList(fontawesome::fa("dna"), " Gene expression"),
            value = "Gene expression",
            div(
              class = "gene-expression-section p-3",
              h3(class = "gene-expression-header mb-4", "Gene Expression Analysis"),
              # Shared filter row
              create_meta_filter_row("_gex"),
              
              # Gene expression controls
              create_plot_controls("data_type_exp", FALSE, "_exp"),
              
              fluidRow(
                id = "gene_exp_container",
                class = "mt-3",
                column(
                  width = 6,
                  id = "dynamic_col_0",
                  subBlock(0)
                )
              )
            )
          ),
          
          # Quality control section
          tabPanel(
            tagList(fontawesome::fa("vial-circle-check"), " Quality control"),
            value = "Quality control",
            create_meta_filter_row("_qc"),
            
            layout_column_wrap(
              width = 1/3,
              class = "mt-3 mb-3",
              uiOutput("total_cells_qc"),
              uiOutput("mean_rna_qc"),
              uiOutput("mean_features_qc")
            ),
            
            div(
              id = "qual_section",
              layout_column_wrap(
                width = 1/2,
                card(
                  full_screen = TRUE,
                  card_header("RNA Count distribution"),
                  withSpinner(
                    type = 6,
                    size = 0.5,
                    color = "#3498db",
                    plotlyOutput("data_count_plotly")
                  )
                ),
                card(
                  full_screen = TRUE,
                  card_header("Feature Count distribution"),
                  withSpinner(
                    type = 6,
                    size = 0.5,
                    color = "#3498db",
                    plotlyOutput("data_feature_plotly")
                  )
                )
              ),
              layout_column_wrap(
                width = 1/2,
                class = "mt-3",
                card(
                  full_screen = TRUE,
                  card_header("Elbow Plot"),
                  plotlyOutput("data_pca_stdv_plotly")
                ),
                card(
                  full_screen = TRUE,
                  card_header("Variable Genes"),
                  plotlyOutput("variable_genes_plotly")
                )
              )
            )
          ),
          
          # Metadata tab
          tabPanel(
            tagList(fontawesome::fa("table-list"), " Metadata"),
            value = "Metadata",
            create_meta_filter_row("_meta"),
            
            layout_column_wrap(
              width = 1/3,
              class = "mt-3 mb-3",
              uiOutput("total_cells_meta"),
              uiOutput("num_studies_meta"),
              uiOutput("num_species_meta")
            ),
            
            div(
              id = "meta_section",
              layout_column_wrap(
                width = 1/3,
                card(card_header("Sample Type"), plotlyOutput("sample_plotly")),
                card(card_header("Host Species"), plotlyOutput("species_plotly")),
                card(card_header("Strain"), plotlyOutput("strain_plotly"))
              ),
              layout_column_wrap(
                width = 1/3,
                class = "mt-3",
                card(card_header("Treatment"), plotlyOutput("treatment_plotly")),
                card(
                  card_header("Parasite Stages"),
                  plotlyOutput("parasite_stages_plotly"),
                  card_footer(
                    class = "text-muted small italic",
                    "Note: All parasite stages are predicted by SingleR, except sporozoites which are confirmed by sample type."
                  )
                ),
                card(card_header("Infection development stage"), plotlyOutput("HPI_plotly"))
              ),
              card(
                class = "mt-3",
                card_header("Data Table"),
                withSpinner(
                  type = 6,
                  size = 0.5,
                  color = "#3498db",
                  DT::DTOutput("data_table")
                )
              )
            )
          ),
          
          # Data sources tab
          tabPanel(
            tagList(fontawesome::fa("book-open"), " Data sources"),
            value = "Data sources",
            card(
              class = "mt-3",
              card_header(tagList(fontawesome::fa("book"), " Data Sources")),
              div(
                id = "source_section",
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
    style = "display: none;",
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
        using the following contact."
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
        visit the GitHub repo:",
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
    fluidRow(id = "particles-js-about", class = "particles-js")
  ),
  
  # Footer
  conditionalPanel(
    condition = "input.home || (!input.analysis && !input.about)", # Show footer only on home page
    div(
      class = "footer",
      div(
        class = "logo-container",
        a(
          href = "https://www.pasteur-kh.org/",
          target = "_blank",
          img(src = "ipc_logo.png", alt = "IPC Logo", class = "logo")
        ),
        img(src = "baia_logo.png", class = "logo")
      ),
      p(class = "copyright", "© 2025 BAIA Team. All rights reserved.")
    )
  )
)
