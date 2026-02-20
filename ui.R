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
        style = "overflow: visible;",
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
                class = "bg-secondary text-white py-1",
                fontawesome::fa("cog", height = "14px"),
                tags$span("Plot settings", class = "fw-bold", style = "font-size: 0.9rem;")
              ),
              card_body(
                fluidRow(
                  class = "g-1 align-items-end",
                  # Data Type - 2 columns
                  tags$div(
                    class = "col-sm-2",
                    style = "flex: 0 0 16.66%; max-width: 16.66%;",
                    tags$div(
                      title = "Select raw or normalized expression data",
                      radioButtons(
                        "data_type",
                        tags$span("Data Type", style = "font-size: 0.8rem; font-weight: 600;"),
                        choices = list("Raw" = "raw", "Normalized" = "normalize"),
                        selected = "normalize",
                        inline = TRUE
                      )
                    )
                  ),
                  # Integration - 1 column
                  tags$div(
                    class = "col-sm-1 d-flex flex-column justify-content-end",
                    style = "flex: 0 0 8.33%; max-width: 8.33%;",
                    conditionalPanel(
                      condition = "input.embedding != 'tsne'",
                      div(
                        class = "form-group shiny-input-container mb-0",
                        tags$label(
                          class = "control-label mb-1",
                          `for` = "check_all",
                          tags$span("Integration", style = "font-size: 0.8rem; font-weight: 600;")
                        ),
                        div(
                          id = "tab_switch",
                          class = "switch-container-compact d-flex align-items-center gap-1",
                          tags$span("Un.", class = "switch-text text-muted", style = "font-size: 0.65rem;"),
                          input_switch("check_all", label = NULL, value = TRUE),
                          tags$span("In.", class = "switch-text", style = "font-size: 0.65rem;")
                        )
                      )
                    )
                  ),
                  # Embedding - 3 columns
                  tags$div(
                    class = "col-sm-3",
                    style = "flex: 0 0 25%; max-width: 25%;",
                    tags$div(
                      title = "Choose the dimensionality reduction method",
                      radioButtons(
                        "embedding",
                        tags$span("Embedding", style = "font-size: 0.8rem; font-weight: 600;"),
                        inline = TRUE,
                        choices = list("PCA" = "pca", "UMAP" = "umap", "tSNE" = "tsne"),
                        selected = "umap"
                      )
                    )
                  ),
                  # Point Size - 2 columns
                  tags$div(
                    class = "col-sm-2",
                    style = "flex: 0 0 16.66%; max-width: 16.66%;",
                    tags$div(
                      title = "Adjust the size of the points in the plot",
                      radioButtons(
                        "size",
                        tags$span("Point Size", style = "font-size: 0.8rem; font-weight: 600;"),
                        choices = list("S" = 2, "M" = 4, "L" = 6),
                        selected = 4,
                        inline = TRUE
                      )
                    )
                  ),
                  # Dimension - 2 columns
                  tags$div(
                    class = "col-sm-2",
                    style = "flex: 0 0 16.66%; max-width: 16.66%;",
                    tags$div(
                      title = "Select 2D or 3D visualization",
                      radioButtons(
                        "plot",
                        tags$span("Dimension", style = "font-size: 0.8rem; font-weight: 600;"),
                        choices = list("2D" = "2d", "3D" = "3d"),
                        selected = "3d",
                        inline = TRUE
                      )
                    )
                  ),
                  # X/Y Selectors - 2 columns
                  tags$div(
                    class = "col-sm-2",
                    style = "flex: 0 0 16.66%; max-width: 16.66%;",
                    conditionalPanel(
                      condition = "input.plot == '2d'",
                      div(
                        class = "xy-selector-stacked",
                        div(class = "d-flex align-items-center gap-1", tags$span("X", style = "font-size: 0.7rem; font-weight: 800;"), tags$div(style = "flex-grow: 1", selectInput("xaxis", NULL, choices = c("pc_1" = "pc_1"), selected = "pc_1", width = "100%"))),
                        div(class = "d-flex align-items-center gap-1", tags$span("Y", style = "font-size: 0.7rem; font-weight: 800;"), tags$div(style = "flex-grow: 1", selectInput("yaxis", NULL, choices = c("pc_2" = "pc_2"), selected = "pc_2", width = "100%")))
                      )
                    )
                  )
                ),
                hr(class = "my-2"),
                # Hidden checkbox to track comparison mode for conditionalPanel
                shinyjs::hidden(checkboxInput("comparison_active", NULL, value = FALSE)),
                div(
                  class = "d-flex justify-content-between align-items-center",
                  # Left side: Color by (only when not comparing)
                  conditionalPanel(
                    condition = "!input.comparison_active",
                    div(
                      class = "d-flex align-items-center gap-3",
                      tags$span("Color by:", style = "font-size: 0.85rem; font-weight: 600; color: #2c3e50;"),
                      selectInput(
                        "color",
                        NULL,
                        choices = list(
                          "Study" = "study_label",
                          "Development phase" = "development_phase",
                          "Parasite stages" = "parasite_stages",
                          "Strain" = "strain",
                          "Treatment" = "treatment",
                          "Sample type" = "sample_type",
                          "Host species" = "host_species",
                          "Genes" = "genes"
                        ),
                        selected = "development_phase",
                        width = "180px"
                      )
                    )
                  ),

                  # Right side: Actions
                  div(
                    class = "ms-auto d-flex align-items-center gap-2",
                    actionButton(
                      "enable_comparison",
                      tagList(fontawesome::fa("columns"), " Compare Plots"),
                      class = "btn-outline-primary shadow-sm rounded-pill px-4 btn-sm fw-bold"
                    ),
                    downloadButton(
                      "download_data", " Download Data",
                      class = "btn-success shadow-sm rounded-pill px-4 btn-sm fw-bold"
                    )
                  )
                ),

                # Gene selection row
                div(
                  id = "genes_single_panel",
                  class = "mt-3 p-3 border rounded bg-white shadow-sm",
                  style = "display: none;",
                  fluidRow(
                    column(
                      width = 5,
                      selectizeInput("genes_n", "Select Genes", choices = "", multiple = TRUE, options = list(placeholder = "Search for genes...", maxItems = 10, dropdownParent = "body"), width = "100%")
                    ),
                    column(
                      width = 7,
                      div(
                        class = "d-flex align-items-center gap-3 mt-4 justify-content-end",
                        input_switch("check", "Show Gene ID", value = FALSE),
                        div(
                          class = "btn-group",
                          tags$a(id = "plasmo_link", href = "#", target = "_blank", class = "btn btn-outline-info btn-sm", tagList(fontawesome::fa("external-link-alt"), " PlasmoDB")),
                          tags$a(id = "ncbi_link", href = "#", target = "_blank", class = "btn btn-outline-info btn-sm", tagList(fontawesome::fa("dna"), " NCBI"))
                        )
                      )
                    )
                  )
                ),

                # Comparison Plot Controls
                div(
                  id = "comparison_controls_panel",
                  class = "mt-2 comparison-controls",
                  style = "display: none;",
                  layout_column_wrap(
                    width = 1 / 2,
                    # Plot A Section
                    div(
                      class = "comparison-plot-section plot-a-section",
                      fluidRow(
                        class = "g-1 align-items-end",
                        column(
                          width = 4,
                          selectInput(
                            "color_first_comp",
                            tags$span("Color by", class = "small fw-semibold"),
                            choices = list(
                              "Study" = "study_label", "Development phase" = "development_phase", "Parasite stages" = "parasite_stages",
                              "Strain" = "strain", "Treatment" = "treatment", "Sample type" = "sample_type",
                              "Host species" = "host_species", "Genes" = "genes"
                            ),
                            selected = "development_phase", width = "100%"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.color_first_comp == 'genes'",
                          column(
                            width = 6,
                            div(
                              class = "gene-select-wrapper",
                              tags$label(
                                class = "control-label",
                                tags$span(class = "small", "Genes"),
                                tags$span(
                                  title = "Click to switch ID and name",
                                  `data-bs-toggle` = "tooltip",
                                  `data-bs-placement` = "top",
                                  bslib::input_switch("check_first_comp", label = NULL, value = FALSE)
                                )
                              ),
                              selectizeInput(
                                "genes_n_first_comp",
                                label = NULL,
                                choices = "", multiple = TRUE,
                                options = list(placeholder = "Search genes...", maxItems = 10, dropdownParent = "body"),
                                width = "100%"
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.color_first_comp == 'genes'",
                          column(
                            width = 2,
                            div(
                              class = "links-row",
                              tags$a(
                                id = "plasmo_link_a",
                                href = "#",
                                target = "_blank",
                                class = "btn btn-outline-primary btn-sm gene-link-btn",
                                `data-bs-toggle` = "tooltip",
                                `data-bs-placement` = "top",
                                title = "PlasmoDB",
                                tags$i(class = "fas fa-external-link-alt")
                              ),
                              tags$a(
                                id = "ncbi_link_a",
                                href = "#",
                                target = "_blank",
                                class = "btn btn-outline-primary btn-sm gene-link-btn",
                                `data-bs-toggle` = "tooltip",
                                `data-bs-placement` = "top",
                                title = "NCBI",
                                tags$i(class = "fas fa-dna")
                              )
                            )
                          )
                        )
                      )
                    ),
                    # Plot B Section
                    div(
                      class = "comparison-plot-section plot-b-section",
                      fluidRow(
                        class = "g-1 align-items-end",
                        column(
                          width = 4,
                          selectInput(
                            "color_second",
                            tags$span("Color by", class = "small fw-semibold"),
                            choices = list(
                              "Study" = "study_label", "Development phase" = "development_phase", "Parasite stages" = "parasite_stages",
                              "Strain" = "strain", "Treatment" = "treatment", "Sample type" = "sample_type",
                              "Host species" = "host_species", "Genes" = "genes"
                            ),
                            selected = "parasite_stages", width = "100%"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.color_second == 'genes'",
                          column(
                            width = 6,
                            div(
                              class = "gene-select-wrapper",
                              tags$label(
                                class = "control-label",
                                tags$span(class = "small", "Genes"),
                                tags$span(
                                  title = "Click to switch ID and name",
                                  `data-bs-toggle` = "tooltip",
                                  `data-bs-placement` = "top",
                                  bslib::input_switch("check_second", label = NULL, value = FALSE)
                                )
                              ),
                              selectizeInput(
                                "genes_second",
                                label = NULL,
                                choices = "", multiple = TRUE,
                                options = list(placeholder = "Search genes...", maxItems = 10, dropdownParent = "body"),
                                width = "100%"
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.color_second == 'genes'",
                          column(
                            width = 2,
                            div(
                              class = "links-row",
                              tags$a(
                                id = "plasmo_link_b",
                                href = "#",
                                target = "_blank",
                                class = "btn btn-outline-success btn-sm gene-link-btn",
                                `data-bs-toggle` = "tooltip",
                                `data-bs-placement` = "top",
                                title = "PlasmoDB",
                                tags$i(class = "fas fa-external-link-alt")
                              ),
                              tags$a(
                                id = "ncbi_link_b",
                                href = "#",
                                target = "_blank",
                                class = "btn btn-outline-success btn-sm gene-link-btn",
                                `data-bs-toggle` = "tooltip",
                                `data-bs-placement` = "top",
                                title = "NCBI",
                                tags$i(class = "fas fa-dna")
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

            # Single plot
            div(
              id = "single_plot_panel",
              card(
                full_screen = TRUE,
                card_header(
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
            div(
              id = "comparison_plots_panel",
              class = "comparison-plots-container mt-2",
              style = "display: none;",
              layout_column_wrap(
                width = 1 / 2,
                height = "600px", # Increased height
                heights_equal = "all",
                card(
                  full_screen = TRUE,
                  class = "comparison-card plot-a-card",
                  card_header(
                    class = "comparison-card-header py-1",
                    div(
                      class = "align-items-center gap-2",
                      fontawesome::fa("circle", fill = "#007bff", height = "12px"),
                      tags$span("Plot A", class = "fw-semibold small"),
                      tags$span(id = "plot_a_subtitle", class = "badge bg-light text-primary border small ms-2")
                    )
                  ),
                  withSpinner(
                    type = 6,
                    size = 0.4,
                    color = "#007bff",
                    plotlyOutput("first_plotly", height = "100%")
                  )
                ),
                card(
                  full_screen = TRUE,
                  class = "comparison-card plot-b-card",
                  card_header(
                    class = "comparison-card-header py-1",
                    div(
                      class = "align-items-center gap-2",
                      fontawesome::fa("circle", fill = "#28a745", height = "12px"),
                      tags$span("Plot B", class = "fw-semibold small"),
                      tags$span(id = "plot_b_subtitle", class = "badge bg-light text-success border small ms-2")
                    )
                  ),
                  withSpinner(
                    type = 6,
                    size = 0.4,
                    color = "#28a745",
                    plotlyOutput("second_plotly", height = "100%")
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
              class = "gene-expression-section",
              style = "overflow: visible;",
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
              width = 1 / 3,
              class = "mt-3 mb-3",
              uiOutput("total_cells_qc"),
              uiOutput("mean_rna_qc"),
              uiOutput("mean_features_qc")
            ),
            div(
              id = "qual_section",
              layout_column_wrap(
                width = 1 / 2,
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
                width = 1 / 2,
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
              width = 1 / 3,
              class = "mt-3 mb-3",
              uiOutput("total_cells_meta"),
              uiOutput("num_studies_meta"),
              uiOutput("num_species_meta")
            ),
            div(
              id = "meta_section",
              layout_column_wrap(
                width = 1 / 3,
                card(card_header("Sample Type"), plotlyOutput("sample_plotly")),
                card(card_header("Host Species"), plotlyOutput("species_plotly")),
                card(card_header("Strain"), plotlyOutput("strain_plotly"))
              ),
              layout_column_wrap(
                width = 1 / 3,
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
    class = "about-section",
    div(
      class = "about-container",
      # Hero Section
      div(
        class = "about-hero",
        div(id = "particles-js-about", class = "particles-js"),
        div(
          class = "about-hero-content",
          h1("About PlaViSca", class = "about-title"),
          p(
            class = "about-subtitle",
            "Plasmodium vivax Single-Cell Analysis Platform"
          )
        )
      ),
      # Main Content
      div(
        class = "about-content",
        # Introduction Card
        card(
          class = "about-card mb-4",
          card_header(
            class = "about-card-header",
            tagList(fontawesome::fa("info-circle", fill = "#008080"), " What is PlaViSca?")
          ),
          card_body(
            p(
              strong("PlaViSca"), " (", em("Plasmodium vivax"), " Single-Cell Analysis) is an interactive web platform designed for exploring and analyzing single-cell RNA sequencing data from ", em("P. vivax"), ", the most widespread human malaria parasite."
            ),
            p(
              "The platform enables researchers to:"
            ),
            tags$ul(
              tags$li("Explore cell embeddings (PCA, UMAP, t-SNE) with interactive visualizations"),
              tags$li("Investigate gene expression patterns across different parasite life stages"),
              tags$li("Filter and analyze metadata across multiple studies"),
              tags$li("Compare different visualization parameters side-by-side")
            ),
            p(
              "This tool aims to accelerate malaria research by providing easy access to curated single-cell transcriptomic data."
            )
          )
        ),
        # Features Section
        div(
          class = "features-section mb-4",
          h3(class = "section-title", tagList(fontawesome::fa("star", fill = "#008080"), " Key Features")),
          fluidRow(
            column(
              width = 3,
              div(
                class = "feature-card",
                div(class = "feature-icon", fontawesome::fa("chart-area", fill = "#007bff")),
                h5("Interactive Plots", class = "feature-title"),
                p("Explore embeddings with zoom, pan, and hover tooltips")
              )
            ),
            column(
              width = 3,
              div(
                class = "feature-card",
                div(class = "feature-icon", fontawesome::fa("dna", fill = "#28a745")),
                h5("Gene Expression", class = "feature-title"),
                p("Visualize expression across parasite life stages")
              )
            ),
            column(
              width = 3,
              div(
                class = "feature-card",
                div(class = "feature-icon", fontawesome::fa("columns", fill = "#ffc107")),
                h5("Side-by-Side Comparison", class = "feature-title"),
                p("Compare different parameters simultaneously")
              )
            ),
            column(
              width = 3,
              div(
                class = "feature-card",
                div(class = "feature-icon", fontawesome::fa("download", fill = "#17a2b8")),
                h5("Data Export", class = "feature-title"),
                p("Download filtered data for further analysis")
              )
            )
          )
        ),
        # Collaboration Card - Enhanced
        div(
          class = "collab-section mb-4",
          h3(class = "section-title", tagList(fontawesome::fa("handshake", fill = "#008080"), " Open for Collaboration")),
          card(
            class = "collab-card",
            card_body(
              fluidRow(
                column(
                  width = 8,
                  h4("We Welcome New Partnerships!", class = "collab-heading"),
                  p(
                    "Are you working with ", em("P. vivax"), " single-cell data? We're actively seeking collaborations to expand our dataset and improve the platform's capabilities."
                  ),
                  tags$ul(
                    tags$li("Integrate your ", em("P. vivax"), " scRNA-seq datasets"),
                    tags$li("Contribute to method development"),
                    tags$li("Share feedback and feature requests"),
                    tags$li("Collaborate on joint publications")
                  )
                ),
                column(
                  width = 4,
                  div(
                    class = "collab-cta",
                    fontawesome::fa("envelope-open-text", fill = "#008080", height = "40px"),
                    h5("Get in Touch"),
                    p("Contact our team to discuss collaboration opportunities.")
                  )
                )
              )
            )
          )
        ),
        # Team Section
        div(
          class = "team-section mb-4",
          h3(class = "section-title", tagList(fontawesome::fa("users", fill = "#008080"), " Research Team")),
          fluidRow(
            column(
              width = 4,
              div(
                class = "team-card",
                div(class = "team-avatar bg-primary", tags$i(class = "fas fa-user")),
                h5("Sopheap Oeng", class = "team-name"),
                p(class = "team-role text-primary", "Bioinformatic System Engineer"),
                p(class = "team-affiliation", "Institut Pasteur du Cambodge"),
                a(href = "mailto:osopheap@pasteur-kh.org", class = "team-email", fontawesome::fa("envelope"), " osopheap@pasteur-kh.org")
              )
            ),
            column(
              width = 4,
              div(
                class = "team-card",
                div(class = "team-avatar bg-success", tags$i(class = "fas fa-user")),
                h5("Anthony Ruberto", class = "team-name"),
                p(class = "team-role text-success", "Postdoctoral Researcher"),
                p(class = "team-affiliation", "University of Georgia, USA"),
                a(href = "mailto:aruberto@uga.edu", class = "team-email", fontawesome::fa("envelope"), " aruberto@uga.edu")
              )
            ),
            column(
              width = 4,
              div(
                class = "team-card",
                div(class = "team-avatar bg-info", tags$i(class = "fas fa-user")),
                h5("Thomas Obadia", class = "team-name"),
                p(class = "team-role text-info", "Bioinformatics & Biostatistics"),
                p(class = "team-affiliation", "Institut Pasteur, Paris"),
                a(href = "mailto:thomas.obadia@pasteur.fr", class = "team-email", fontawesome::fa("envelope"), " thomas.obadia@pasteur.fr")
              )
            )
          ),
          fluidRow(
            class = "mt-3",
            column(
              width = 4,
              div(
                class = "team-card",
                div(class = "team-avatar bg-warning", tags$i(class = "fas fa-user")),
                h5("Brice Feufack Donfack", class = "team-name"),
                p(class = "team-role text-warning", "Postdoctoral Researcher"),
                p(class = "team-affiliation", "Malaria Research Unit, IPC"),
                a(href = "mailto:ldonfack@pasteur-kh.org", class = "team-email", fontawesome::fa("envelope"), " ldonfack@pasteur-kh.org")
              )
            ),
            column(
              width = 4,
              div(
                class = "team-card",
                div(class = "team-avatar bg-danger", tags$i(class = "fas fa-user-tie")),
                h5("Giorgio Gonnella", class = "team-name"),
                p(class = "team-role text-danger", "Head, BAIA Unit"),
                p(class = "team-affiliation", "Institut Pasteur du Cambodge"),
                a(href = "mailto:ggonnella@pasteur-kh.org", class = "team-email", fontawesome::fa("envelope"), " ggonnella@pasteur-kh.org")
              )
            )
          )
        ),
        # Source Code Section - Separate
        div(
          class = "source-section",
          card(
            class = "source-card",
            card_body(
              fluidRow(
                column(
                  width = 2,
                  div(class = "source-icon-wrapper", fontawesome::fa("github", fill = "#333", height = "50px"))
                ),
                column(
                  width = 7,
                  h4("Open Source", class = "source-title"),
                  p(
                    "PlaViSca is built with R Shiny and is available as open-source software. Explore the code, report issues, or contribute to the project on GitHub."
                  )
                ),
                column(
                  width = 3,
                  a(
                    href = "https://github.com/Sopheap15/PlaViSca",
                    target = "_blank",
                    class = "btn btn-dark btn-lg source-btn",
                    fontawesome::fa("github"), " View on GitHub"
                  )
                )
              )
            )
          )
        ),
        # Affiliations Section
        div(
          class = "affiliations-section mt-4",
          h3(class = "section-title", tagList(fontawesome::fa("building", fill = "#008080"), " Affiliations")),
          fluidRow(
            column(
              width = 4,
              div(
                class = "affiliation-card",
                h5("BAIA Unit"),
                p("Bioinformatics and AI Applications"),
                tags$small("Institut Pasteur du Cambodge")
              )
            ),
            column(
              width = 4,
              div(
                class = "affiliation-card",
                h5("Malaria Unit"),
                p("Malaria Research"),
                tags$small("Institut Pasteur du Cambodge")
              )
            ),
            column(
              width = 4,
              div(
                class = "affiliation-card",
                h5("CTEGD"),
                p("Center for Tropical & Emerging Global Diseases"),
                tags$small("University of Georgia, USA")
              )
            )
          )
        )
      )
    )
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
