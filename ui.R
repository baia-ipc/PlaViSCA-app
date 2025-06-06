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
      div(class = "logo mosquito", img(src = "mosquito-top.png", alt = "mosquito")),
      div(
        class = "title-text",
        h1("PlaViSca"),
        span("Plasmodium vivax Single Cell Analysis Platform")
      )
    ),
    div(
      class = "menu-items",
      actionLink("home",
        label = tagList(icon("home"), " Home"),
        class = "menu-link active"
      ),
      span("|"),
      actionLink("analysis",
        label = tagList(icon("table"), " Analysis"),
        class = "menu-link"
      ),
      span("|"),
      actionLink("about",
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
        width = 6, class = "intro-left",
        # h2("PlaViSca Platform"),
        p("PlaViSCA is a platform designed to support the malaria research community
        in analyzing and visualizing single-cell RNA sequencing (scRNA-seq) data.
        PvSCA provides a user friendly interface for exploring
        metadata and gene expression across various parasite life cycle stages
        using interactive tables and 2D or 3D visualizations with customizable
        filters and coloring.The platform also allows users to download data for
        further analysis. PlaViSCA empowers researchers to explore complex scRNA-seq for
        P. vivax without requiring any programming skills."),
        # actionLink("explore_data",
        #   label = tagList(icon("hand-point-right"), "Explore data"),
        #   class = "btn btn-outline-light btn-sm explore-btn"
        # ),

        div(class = "logo mosquito-bottom", img(
          src = "mosquito-bottom.png",
          alt = "mosquito"
        ))
      ),
      column(
        width = 6, class = "intro-right",
        div(
          class = "pages_link",
          actionLink("metadata-link", tagList(
            img(src = "metadata.png", alt = "Metadata"),
            span("Metadata")
          )),
          actionLink("analysis-link", tagList(
            img(src = "umap.png", alt = "Analysis"),
            span("Analysis")
          )),
          actionLink("qc-link", tagList(
            img(src = "qc.png", alt = "Quality control"),
            span("Quality control")
          ))
        ),
        div(
          class = "pages_link",
          actionLink("sources-link", tagList(
            img(src = "data_sources.png", alt = "Data sources"),
            span("Data sources")
          )),
          actionLink("gene_exp-link", tagList(
            img(src = "gene_exp.png", alt = "Gene Expression"),
            span("Gene exp")
          )),
          actionLink("contribute-link", tagList(
            img(src = "contribute.png", alt = "Contribute"),
            span("Contribute")
          ))
        ),
        div(
          class = "pages_link",
          actionLink("usermanual-link", tagList(
            img(src = "usermanual.png", alt = "User Manual"),
            span("User Manual")
          )),
          actionLink("sources-code-link", tagList(
            img(src = "sources_code.png", alt = "Sources code"),
            span("Source code")
          )),
          actionLink("about-link", tagList(
            img(src = "about.png", alt = "About"),
            span("About")
          ))
        )

        # h2("Plasmodium vivax life cycle"),
        # div(class = "img", img(src = "pvivax.jpg", usemap = "#malariaMap"))
      )
    ),
    fluidRow(id = "particles-js-home", class = "particles-js"),
    fluidRow(p(class = "copyright", "© 2025 BAIA Team. All rights reserved."))
    # tags$map(
    #   name = "malariaMap",
    #   tags$area(
    #     shape = "rect", coords = "1090, 1774, 1408, 1843",
    #     href = "javascript:void(0)", alt = "hypnozoite", title = "hypnozoite"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "677, 2037, 1006, 2190",
    #     href = "javascript:void(0)", alt = "merozoite", title = "merozoite"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "1021, 2205, 1416, 2348",
    #     href = "javascript:void(0)", alt = "merozoite", title = "merozoite"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "1639, 2041, 1778, 2143",
    #     href = "javascript:void(0)", alt = "ring", title = "ring"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "1767, 1902, 2100, 1956",
    #     href = "javascript:void(0)", alt = "trophozoite", title = "trophozoite"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "2173, 2121, 1946, 2048",
    #     href = "javascript:void(0)", alt = "schizont", title = "schizont"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "2122, 2370, 1895, 2234",
    #     href = "javascript:void(0)", alt = "schizont", title = "schizont"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "2382, 1810, 3142, 1894",
    #     href = "javascript:void(0)", alt = "gametocyte", title = "gametocyte"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "2466, 2179, 3080, 2242",
    #     href = "javascript:void(0)", alt = "gametocyte", title = "gametocyte"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "3011, 1298, 3278, 1375",
    #     href = "javascript:void(0)", alt = "gametocyte", title = "gametocyte"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "3007, 325, 3424, 413",
    #     href = "javascript:void(0)", alt = "Oocyst", title = "Oocyst"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "2016, 790, 2330, 878",
    #     href = "javascript:void(0)", alt = "sporozoite", title = "sporozoite"
    #   ),
    #   tags$area(
    #     shape = "rect", coords = "779, 1232, 1094, 1305",
    #     href = "javascript:void(0)", alt = "sporozoite", title = "sporozoite"
    #   )
    # )
  ),

  # Analysis section
  div(
    id = "analysis_section",
    fluidRow(
      class = "analysis-container",
      column(
        width = 4, id = "sidebar", class = "sidebar",
        column(
          width = 12,
          checkboxGroupInput(
            "stagei", "Infection stage",
            choices = list(
              "Blood" = "blood",
              "Liver" = "liver",
              "Sporozoite" = "sporozoite"
            ),
            selected = list("blood", "liver", "sporozoite")
          )
        ),
        column(
          width = 12,
          checkboxGroupInput(
            "stagelr", "Parasite stage",
            choices = list(
              "Trophozoite" = "trophozoite",
              "Ring" = "ring",
              "Schizont" = "schizont",
              "Gametocyte" = "gametocyte",
              "Hypnozoite" = "hypnozoite",
              "Hepatic schizont" = "hepatic_schizont",
              "Zygot" = "zygot",
              "Ookinete" = "ookinete",
              "Oocyst" = "oocyst",
              "Sporozoite" = "sporozoite"
            ),
            selected = list()
          )
        ),
        column(width = 12, radioButtons("size", "Size",
          choices = list("S" = 2, "M" = 4, "L" = 6),
          selected = 4
        )),
        column(width = 12, radioButtons("color", "Color",
          choices = list(
            "Identity" = "ident",
            "Stage" = "stage",
            "Strain" = "vivax_strain",
            "Day" = "day",
            "Condition" = "condition",
            "Replicate" = "replicate",
            "Species" = "species",
            "CCA_snn" = "cca_snn",
            "Genes" = "genes"
          ),
          selected = "ident"
        )),
        column(
          width = 12,
          fluidRow(
            column(
              width = 12,
              radioButtons("data_type", "Data type",
                choices = list(
                  "Raw" = "raw",
                  "Normalized" = "normalize",
                  "Scaled" = "scale"
                ),
                selected = "normalize"
              )
            )
          ),
          fluidRow(
            id = "gene_snn_ui",
            column(
              width = 7,
              selectizeInput("genes_n", "Gene",
                choices = ""
              )
            ),
            column(
              id = "switch_id",
              width = 2,
              tags$label(
                class = "switch",
                tags$input(id = "check", type = "checkbox"),
                tags$span(class = "slider")
              )
            ),
            column(
              width = 5,
              tags$div(
                id = "gene_link_ui",
                tags$a(
                  id = "plasmo_link", href = "#",
                  target = "_blank", "🔬 PlasmoDB"
                ),
                tags$a(
                  id = "ncbi_link", href = "#",
                  target = "_blank", "🧬 NCBI"
                )
              )
            )
          )
        ),
        column(width = 12, radioButtons("plot", "Plot",
          choices = list("2D" = "2d", "3D" = "3d"),
          selected = "2d"
        )),
        column(
          width = 12,
          fluidRow(
            id = "axis",
            column(
              width = 6, id = "xaxis-container",
              selectInput("xaxis", "X-axis",
                choices = c("pc_1"),
                selected = "pc_1"
              )
            ),
            column(
              width = 6, id = "yaxis-container",
              selectInput("yaxis", "Y-axis",
                choices = c("pc_2"),
                selected = "pc_2"
              )
            )
          )
        ),
        column(width = 12, downloadButton("download_data", "Download",
          class = "bt-download"
        ))
      ),
      column(
        width = 8, id = "main", class = "main",
        tabsetPanel(
          id = "tabset",
          # Metadata tab
          tabPanel(
            "Metadata",
            div(
              id = "meta_section",
              fluidRow(
                column(
                  width = 2,
                  selectizeInput("ident", "Ident:",
                    choices = c(""),
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select options...",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                column(
                  width = 2,
                  selectizeInput("species", "Species:",
                    choices = c(""),
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select options...",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                column(
                  width = 2,
                  selectizeInput("type", "Type:",
                    choices = c(""),
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select options...",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                column(
                  width = 2,
                  selectizeInput("condition", "Condition:",
                    choices = c(""),
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select options...",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                column(
                  width = 2,
                  selectizeInput("stage", "Stage:",
                    choices = c(""),
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select options...",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                column(
                  width = 2,
                  selectizeInput("sample", "Sample:",
                    choices = c(""),
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select options...",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                )
              ),
              br(),
              fluidRow(
                column(width = 4, plotlyOutput("stage_plotly")),
                column(width = 4, plotlyOutput("sample_plotly")),
                column(width = 4, plotlyOutput("strain_plotly"))
              ),
              fluidRow(
                column(width = 4, plotlyOutput("species_plotly")),
                column(width = 4, plotlyOutput("type_plotly")),
                column(width = 4, plotlyOutput("location_plotly"))
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
            "Quality control",
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
          tabPanel(
            "Data sources",
            div(
              id = "source_section",
              column(
                width = 12,
                DT::DTOutput("data_source")
              )
            )
          ),

          # gene expression section
          tabPanel(
            "Gene expression",
            br(),
            fluidRow(
              column(
                width = 12,
                radioButtons(
                  "data_type_exp", "Data type",
                  choices = list(
                    "Raw" = "raw",
                    "Normalized" = "normalize",
                    "Scaled" = "scale"
                  ),
                  selected = "normalize"
                )
              )
            ),
            fluidRow(
              column(
                id = "gene_exp_tar",
                width = 6,
                fluidRow(
                  column(
                    width = 6,
                    selectizeInput("genes_exp_0", "Gene", choices = NULL),
                    column(
                      width = 2,
                      class = "switch_exp",
                      tags$label(
                        class = "switch",
                        tags$input(
                          id = "check_exp_0", type = "checkbox",
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
                          id = "plasmo_link_exp", href = "#",
                          target = "_blank", "🔬 PlasmoDB"
                        ),
                        span("|"),
                        tags$a(
                          id = "ncbi_link_exp", href = "#",
                          target = "_blank", "🧬 NCBI"
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    selectInput("group_exp", "Group by",
                      choices = group_choices,
                      selected = "Stage"
                    )
                  )
                ),
                fluidRow(
                  width = 12,
                  plotlyOutput("gene_exp_plotly"),
                  fluidRow(
                    class = "btn-add-rm-container",
                    actionButton("add_btn_0",
                      label = icon("plus"),
                      class = "btn btn-success btn_add"
                    )
                  )
                )
              )
            ),
            div(id = "gene_exp_container")
          ),

          # embedding section
          tabPanel(
            "Embeddings",
            fluidRow(
              column(
                id = "tab_switch",
                width = 5,
                tags$span(class = "left-label", "Unintegrated"),
                tags$label(
                  class = "switch",
                  tags$input(id = "check_all", type = "checkbox"),
                  tags$span(class = "slider")
                ),
                tags$span(class = "right-label", "Integrated")
              ),
              column(
                width = 6,
                class = "radio",
                radioButtons(
                  "embedding", NULL,
                  inline = TRUE,
                  choices = list(
                    "PCA" = "pca",
                    "UMAP" = "umap",
                    "tSNE" = "tsne"
                  ),
                  selected = "pca"
                )
              )
            ),
            br(),
            fluidRow(
              withSpinner(
                type = 6,
                size = 0.5,
                color = "#3498db",
                plotlyOutput("all_plotly")
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
        h2("Collaborators"),
        p("PlaViSca was developed by the Bioinformatics and Artificial Intelligence
       Applications (BAIA) Unit at the Institut Pasteur du Cambodge (IPC),
       in collaboration with the Malaria Unit at IPC and the Center for Tropical
       and Emerging Global Diseases at the University of Georgia, USA."),
        p("We are open to collaboration and welcome opportunities to integrate
       additional relevant data into this platform. Please reach out to us
       using following contact.")
      ),
      column(
        width = 4,
        id = "colab-particles",
        class = "collab-particles"
      )
    ),
    fluidRow(
      class = "contact",
      h2("Contact detail:"),
      tags$ul(
        tags$li(
          strong("Sopheap OENG"), " - Bioinformatic System Engineer, IPC, email: ",
          a(href = "mailto:osopheap@pasteur-kh.org", "osopheap@pasteur-kh.org")
        ),
        tags$li(
          strong("Anthony RUBERTO"), " - Postdoctoral researcher at the Center
          for Tropical and Emerging Global Diseases, University of Georgia, USA,
          email: ", a(href = "mailto:aruberto@uga.edu", "aruberto@uga.edu")
        ),
        tags$li(
          strong("Jean POPOVICI"), " - Head, Malaria Research Unit, IPC, email: ",
          a(href = "mailto:jpopovici@pasteur-kh.org", "jpopovici@pasteur-kh.org")
        ),
        tags$li(
          strong("Giorgio GONNELLA"), " - Head, BAIA Unit, IPC, email: ",
          a(href = "mailto:ggonella@pasteur-kh.org", "ggonella@pasteur-kh.org")
        )
      ),
      p(
        "PlaViSca is built using the R Shiny framework. For more detail, please
        visit GitHub repo:", a(
          href = "https://github.com/Sopheap15/pvsca",
          HTML('https://github.com/Sopheap15/pvsca
               <span class="point-emoji">👈 </span>'), target = "_blank"
        ), "."
      )
    ),
    fluidRow(id = "particles-js-about", class = "particles-js"),
    fluidRow(p(class = "copyright", "© 2025 BAIA Team. All rights reserved."))
  )
)
