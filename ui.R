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
    tags$link(
      rel = "stylesheet",
      href = "https://code.jquery.com/ui/1.13.2/themes/smoothness/jquery-ui.css"
    ),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap",
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
    div(id = "particles-js-analysis", class = "particles-js-full"),
    div(
      class = "analysis-dashboard",
      # Main Content for Analysis
      div(
        class = "analysis-content-wrapper",
        div(
          class = "analysis-content w-100",
          style = "overflow: visible; position: relative; z-index: 1;",
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
                class = "mb-2 shadow-sm settings-card-compact",
                card_header(
                  class = "bg-primary text-white py-1",
                  fontawesome::fa("cog", height = "14px"),
                  tags$span("Plot settings", class = "fw-bold", style = "font-size: 0.9rem;")
                ),
                card_body(
                  fluidRow(
                    class = "g-1 align-items-end mb-n2",
                    # Data Type - 2 columns
                    tags$div(
                      class = "col-sm-2 col-custom-2",
                      tags$div(
                        title = "Select raw or normalized expression data",
                        radioButtons(
                          "data_type",
                          tags$span("Data Type", class = "settings-label"),
                          choices = list("Raw" = "raw", "Normalized" = "normalize"),
                          selected = "normalize",
                          inline = TRUE
                        )
                      )
                    ),
                    # Integration - 1 column
                    tags$div(
                      class = "col-sm-1 col-custom-1 d-flex flex-column justify-content-end",
                      conditionalPanel(
                        condition = "input.embedding != 'tsne'",
                        div(
                          class = "form-group shiny-input-container mb-0",
                          tags$label(
                            class = "control-label",
                            `for` = "check_all",
                            tags$span("Integration", class = "settings-label", style = "margin-bottom: -5px;")
                          ),
                          div(
                            id = "tab_switch",
                            class = "switch-container-compact d-flex align-items-center gap-1",
                            tags$span("Un.", class = "switch-text text-muted switch-text-small"),
                            input_switch("check_all", label = NULL, value = TRUE),
                            tags$span("In.", class = "switch-text switch-text-small")
                          )
                        )
                      )
                    ),
                    # Embedding - 3 columns
                    tags$div(
                      class = "col-sm-3 col-custom-3",
                      tags$div(
                        title = "Choose the dimensionality reduction method",
                        radioButtons(
                          "embedding",
                          tags$span("Embedding", class = "settings-label"),
                          inline = TRUE,
                          choices = list("PCA" = "pca", "UMAP" = "umap", "tSNE" = "tsne"),
                          selected = "umap"
                        )
                      )
                    ),
                    # Point Size - 2 columns
                    tags$div(
                      class = "col-sm-2 col-custom-2",
                      tags$div(
                        title = "Adjust the size of the points in the plot",
                        radioButtons(
                          "size",
                          tags$span("Point Size", class = "settings-label"),
                          choices = list("S" = 2, "M" = 4, "L" = 6),
                          selected = 4,
                          inline = TRUE
                        )
                      )
                    ),
                    # Dimension - 2 columns
                    tags$div(
                      class = "col-sm-2 col-custom-2",
                      tags$div(
                        title = "Select 2D or 3D visualization",
                        radioButtons(
                          "plot",
                          tags$span("Dimension", class = "settings-label"),
                          choices = list("2D" = "2d", "3D" = "3d"),
                          selected = "3d",
                          inline = TRUE
                        )
                      )
                    ),
                    # X/Y Selectors - 2 columns
                    tags$div(
                      class = "col-sm-2 col-custom-2",
                      conditionalPanel(
                        condition = "input.plot == '2d'",
                        div(
                          class = "xy-selector-horizontal",
                          div(class = "d-flex align-items-center gap-1", tags$span("X", class = "xy-label"), tags$div(selectInput("xaxis", NULL, choices = c("pc_1" = "pc_1"), selected = "pc_1", width = "80px"))),
                          div(class = "d-flex align-items-center gap-1", tags$span("Y", class = "xy-label"), tags$div(selectInput("yaxis", NULL, choices = c("pc_2" = "pc_2"), selected = "pc_2", width = "80px")))
                        )
                      )
                    )
                  ),
                  # Removed HR to save space
                  shinyjs::hidden(checkboxInput("comparison_active", NULL, value = FALSE)),
                  div(
                    class = "d-flex justify-content-between align-items-center mt-n1",
                    # Left side: Color by (only when not comparing)
                    conditionalPanel(
                      condition = "!input.comparison_active",
                      div(
                        class = "d-flex align-items-center gap-3",
                        tags$span("Color by:", class = "color-by-label"),
                        selectizeInput(
                          "color",
                          NULL,
                          choices = category_choices(),
                          selected = get_data_column("broad_stage"),
                          width = "180px",
                          options = list(dropdownParent = "body")
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
                    class = "mt-2 p-2 border rounded bg-white shadow-sm",
                    style = "display: none;",
                    fluidRow(
                      class = "g-1 align-items-end",
                      column(
                        width = 10,
                        div(
                          class = "gene-select-wrapper",
                          style = "position: relative;",
                          tags$label(
                            class = "control-label d-flex align-items-center gap-2",
                            tags$span(textOutput("genes_label_single", inline = TRUE), class = "settings-label"),
                            div(
                              class = "single-toggle-container",
                              title = "Click to switch ID and name",
                              `data-bs-toggle` = "tooltip",
                              `data-bs-placement` = "top",
                              input_switch("check", label = NULL, value = FALSE)
                            )
                          ),
                          selectizeInput(
                            "genes_n",
                            label = NULL,
                            choices = "", multiple = TRUE,
                            options = list(placeholder = "Search for genes...", maxItems = 10, dropdownParent = "body"),
                            width = "100%"
                          )
                        )
                      ),
                      column(
                        width = 2,
                        div(
                          class = "links-row",
                          tags$a(
                            id = "plasmo_link",
                            href = "#",
                            target = "_blank",
                            class = "btn btn-outline-info btn-sm gene-link-btn",
                            `data-bs-toggle` = "tooltip",
                            `data-bs-placement` = "top",
                            title = "PlasmoDB",
                            tags$i(class = "fas fa-external-link-alt")
                          ),
                          tags$a(
                            id = "ncbi_link",
                            href = "#",
                            target = "_blank",
                            class = "btn btn-outline-info btn-sm gene-link-btn",
                            `data-bs-toggle` = "tooltip",
                            `data-bs-placement` = "top",
                            title = "NCBI",
                            tags$i(class = "fas fa-dna")
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
                            selectizeInput(
                              "color_first_comp",
                              tags$span("Color by", class = "small fw-semibold"),
                              choices = category_choices(),
                              selected = get_data_column("broad_stage"), width = "100%",
                              options = list(dropdownParent = "body")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.color_first_comp == 'genes'",
                            column(
                              width = 6,
                              div(
                                class = "gene-select-wrapper",
                                style = "position: relative;",
                                tags$label(
                                  class = "control-label d-flex align-items-center gap-2",
                                  tags$span(textOutput("genes_label_first_comp", inline = TRUE), class = "settings-label"),
                                  div(
                                    class = "comparison-toggle-container-a",
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
                            selectizeInput(
                              "color_second",
                              tags$span("Color by", class = "small fw-semibold"),
                              choices = category_choices(),
                              selected = get_data_column("detailed_stage"), width = "100%",
                              options = list(dropdownParent = "body")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.color_second == 'genes'",
                            column(
                              width = 6,
                              div(
                                class = "gene-select-wrapper",
                                style = "position: relative;",
                                tags$label(
                                  class = "control-label d-flex align-items-center gap-2",
                                  tags$span(textOutput("genes_label_second", inline = TRUE), class = "settings-label"),
                                  div(
                                    class = "comparison-toggle-container-b",
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
                  height = "650px",
                  card_header(
                    "Visualization"
                  ),
                  withSpinner(
                    type = 6,
                    size = 0.5,
                    color = "#3498db",
                    plotlyOutput("all_plotly", height = "100%")
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
                class = "gene-expression-section py-2", # Added py-2 for less padding
                style = "overflow: visible;",
                # Removed Header
                # Shared filter row
                create_meta_filter_row("_gex"),

                # Gene expression controls
                create_plot_controls("data_type_exp", FALSE, "_exp"),

                # Note: gene coverage differs between studies
                div(
                  class = "alert alert-info py-2 px-3 mt-2 mb-0 small",
                  fontawesome::fa("circle-info", height = "12px"),
                  tags$span(class = "ms-1", app_note("gene_expression_coverage"))
                ),
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
                  card(
                    card_header(get_data_label("broad_stage")),
                    plotlyOutput("broad_stage_plotly")
                  ),
                  card(
                    card_header(get_data_label("detailed_stage")),
                    plotlyOutput("detailed_stage_plotly"),
                    card_footer(
                      class = "text-muted small italic",
                      app_note("stage_prediction")
                    )
                  ),
                  card(card_header(get_data_label("host_species")), plotlyOutput("species_plotly"))
                ),
                layout_column_wrap(
                  width = 1 / 3,
                  class = "mt-3",
                  card(
                    card_header(get_data_label("parasite_lineage_isolate")),
                    plotlyOutput("lineage_plotly")
                  ),
                  card(
                    card_header(get_data_label("treatment")),
                    plotlyOutput("treatment_plotly"),
                    card_footer(
                      class = "text-muted small italic",
                      get_data_help("treatment")
                    )
                  ),
                  card(
                    card_header(get_data_label("idc_reference_class")),
                    plotlyOutput("idc_reference_class_plotly"),
                    card_footer(
                      class = "text-muted small italic",
                      get_data_help("idc_reference_class")
                    )
                  )
                ),
                layout_column_wrap(
                  width = 1 / 3,
                  class = "mt-3",
                  card(
                    card_header(get_data_label("parasite_origin_location")),
                    plotlyOutput("origin_location_plotly"),
                    card_footer(
                      class = "text-muted small italic",
                      get_data_help("parasite_origin_location")
                    )
                  ),
                  card(
                    card_header(get_data_label("pred_gametocyte_sex")),
                    plotlyOutput("pred_gametocyte_sex_plotly"),
                    card_footer(
                      class = "text-muted small italic",
                      get_data_help("pred_gametocyte_sex")
                    )
                  ),
                  card(
                    card_header(get_data_label("source_sex_annotation")),
                    plotlyOutput("source_sex_annotation_plotly"),
                    card_footer(
                      class = "text-muted small italic",
                      get_data_help("source_sex_annotation")
                    )
                  )
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
    )
  ),
  div(
    id = "about_section",
    style = "display: none;",
    class = "about-section",
    div(
      class = "about-container",
      # Hero Section
      div(
        class = "about-page-wrapper",
        div(id = "particles-js-about", class = "particles-js-full"),
        div(
          class = "about-content-minimal",
          # Introduction
          div(
            class = "text-center mb-5",
            h1("What is PlaViSca?", class = "minimal-title"),
            p(class = "minimal-subtitle", tags$em("Plasmodium vivax"), " Single-Cell Analysis Platform")
          ),
          div(
            class = "about-section-condensed mb-5",
            p(
              strong("PlaViSca"), " is a high-performance platform for the multi-dimensional exploration of ", em("P. vivax"), " single-cell transcriptomics. We provide researchers with the tools to translate complex genomic data into impactful biological insights."
            )
          ),

          # Capabilities
          div(
            class = "about-section-condensed mb-5",
            h3(class = "minimal-section-header", "Key Capabilities"),
            tags$ul(
              class = "minimal-list",
              tags$li(strong("professional embeddings:"), " Interactive navigation of UMAP, PCA, and t-SNE projections."),
              tags$li(strong("gene expression:"), " High-resolution visualization of expression across life stages."),
              tags$li(strong("unified metadata:"), " Centralized access to curated multi-study datasets."),
              tags$li(strong("comparative analytics:"), " Advanced side-by-side analysis of biological states.")
            )
          ),

          # Team Section - Professional Cards
          div(
            class = "about-section-condensed mb-5",
            h3(class = "minimal-section-header", "Development Team"),
            div(
              class = "team-grid-professional",
              div(
                class = "team-card-prof accent-teal",
                h5("Sopheap Oeng", class = "team-name-prof"),
                span("bioinformatic system engineer", class = "team-role-prof"),
                a(href = "mailto:osopheap@pasteur-kh.org", class = "team-email-prof", fontawesome::fa("envelope"), " osopheap@pasteur-kh.org")
              ),
              div(
                class = "team-card-prof accent-blue",
                h5("Lionel Brice Feufack-Donfack", class = "team-name-prof"),
                span("postdoctoral researcher", class = "team-role-prof"),
                a(href = "mailto:ldonfack@pasteur-kh.org", class = "team-email-prof", fontawesome::fa("envelope"), " ldonfack@pasteur-kh.org")
              ),
              div(
                class = "team-card-prof accent-green",
                h5("Thomas Obadia", class = "team-name-prof"),
                span("senior scientist", class = "team-role-prof"),
                a(href = "mailto:tobadia@pasteur-kh.org", class = "team-email-prof", fontawesome::fa("envelope"), " tobadia@pasteur-kh.org")
              ),
              div(
                class = "team-card-prof accent-purple",
                h5("Anthony Ruberto", class = "team-name-prof"),
                span("senior scientist", class = "team-role-prof"),
                a(href = "mailto:aruberto02@gmail.com", class = "team-email-prof", fontawesome::fa("envelope"), " aruberto02@gmail.com")
              ),
              div(
                class = "team-card-prof accent-orange",
                h5("Giorgio Gonnella", class = "team-name-prof"),
                span("Head, BAIA Unit", class = "team-role-prof"),
                a(href = "mailto:ggonnella@pasteur-kh.org", class = "team-email-prof", fontawesome::fa("envelope"), " ggonnella@pasteur-kh.org")
              )
            )
          ),

          # Collaboration & Open Science Section
          div(
            class = "collaboration-box",
            h3("Collaboration & Open Science", class = "collab-header"),
            div(
              class = "collab-grid",
              div(
                class = "collab-item",
                fontawesome::fa("microscope"),
                h4("open source mission"),
                p("open science is at our core. we believe in transparent, reproducible research and provide open source tools to accelerate biological discovery globally.")
              ),
              div(
                class = "collab-item",
                fontawesome::fa("database"),
                h4("dataset integrations"),
                p("we welcome raw and processed dataset submissions to expand our multi-dimensional view of ", tags$em("P. vivax"), " single-cell biology.")
              ),
              div(
                class = "collab-item",
                fontawesome::fa("globe"),
                h4("global partnerships"),
                p("PlaViSca thrives on international collaboration. we invite researchers and institutions to join our network to extend the platform and seek funding to support these activities.")
              )
            ),
            div(
              class = "github-footer-minimal",
              a(
                href = "https://github.com/Sopheap15/PlaViSca",
                target = "_blank",
                class = "github-link-beautiful",
                fontawesome::fa("github"), " Explore Source Code on GitHub"
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
