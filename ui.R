library(shiny)
library(shinyjs)
library(plotly)
library(shinycssloaders)

ui <- fluidPage(
  tags$head(
    useShinyjs(),
    includeCSS("www/style.css"),
    tags$script(src = "script.js"),
    tags$script(src = "jquery.rwdImageMaps.min.js")
  ),

  # Menubar
  div(
    class = "menu-bar",
    div(class = "logo", img(src = "Pvivax.jpeg", height = "60px")),
    div(
      class = "menu-title",
      span("🦟 Plasmodium vivax 🦟")
    ),
    div(
      class = "menu-items",
      actionLink("about",
        label = tagList(icon("info-circle"), " About"),
        class = "menu-link"
      ),
      span("|"),
      actionLink("home",
        label = tagList(icon("home"), " Home"),
        class = "menu-link"
      ),
      span("|"),
      actionLink("data",
        label = tagList(icon("table"), " Explore data"),
        class = "menu-link"
      )
    )
  ),

  # About section
  div(
    id = "about_section",
    fluidRow(
      class = "about",
      h2("About the team"),
      p(
        "Plasmodium vivax Single Cell Analysis (PvSCA) Platform was developed by
        the Bioinformatics and Artificial Intelligence Applications (BAIA) Unit
        at the Institut Pasteur du Cambodge (IPC), in collaboration with the
        Malaria Unit at IPC and the Center for Tropical and Emerging Global
        Diseases at the University of Georgia, USA."
      ),
      p("The team can be contacted at the following:"),
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
        "This web application is built using the R Shiny framework to provide an
        interactive and dynamic web interface."
      ),
      p(
        "For more detailed information, please visit the project's GitHub",
        "repository at ",
        a(
          href = "https://github.com/Sopheap15/pvsca",
          HTML('https://github.com/Sopheap15/pvsca
               <span class="point-emoji">👈 </span>'), target = "_blank"
        ), "."
      ),
      p("© 2025 BAIA Team. All rights reserved.")
    )
  ),

  # Home section
  div(
    id = "home_section",
    fluidRow(
      class = "introduction",
      p("The Plasmodium vivax Single Cell Analysis (PvSCA) platform is a web-based
        application developed to unlock the full potential of single-cell RNA
        sequencing (scRNA-seq) data in malaria research. Traditionally, accessing
        and interpreting these datasets required specialized bioinformatics skills,
        limiting their utility to a subset of researchers. PvSCA addresses this
        challenge by offering an easy-to-use, interactive interface that provides
        free access to high-resolution transcriptomic data from P. vivax,
        empowering scientists to explore parasite biology at the single-cell
        level—without writing any code."),
      p("More than just a data viewer, PvSCA offers a suite of analytical tools
        that allow users to examine P. vivax gene expression across developmental
        stages and experimental conditions. Through dynamic dimensional reduction
        visualizations, researchers can identify specific transcriptomic expression
        patterns, track parasite differentiation stages—such as rings,
        trophozoites, and others—and compare trends across multiple studies.
        The platform also enables users to harmonize and share data for downstream
        analysis and collaboration."),
      p(
        "PvSCA is designed to serve a multidisciplinary community—from basic
        scientists investigating parasite gene regulation to translational
        researchers developing diagnostics and vaccines. It also supports
        educational and public health efforts, providing a clear and visual
        introduction to P. vivax biology for students, educators, and
        policymakers.",
        actionLink("explore_data", "Explore data >>")
      ),
      p("To better understand the context, users can explore the Plasmodium vivax
      life cycle in the image below.")
    ),
    div(class = "img", img(
      src = "pvivax.jpg", usemap = "#malariaMap",
      style = "width: 100%; height: auto;"
    )),
    tags$map(
      name = "malariaMap",
      tags$area(
        shape = "rect", coords = "1090, 1774, 1408, 1843",
        href = "javascript:void(0)", alt = "hypnozoite", title = "hypnozoite"
      ),
      tags$area(
        shape = "rect", coords = "677, 2037, 1006, 2190",
        href = "javascript:void(0)", alt = "merozoite", title = "merozoite"
      ),
      tags$area(
        shape = "rect", coords = "1021, 2205, 1416, 2348",
        href = "javascript:void(0)", alt = "merozoite", title = "merozoite"
      ),
      tags$area(
        shape = "rect", coords = "1639, 2041, 1778, 2143",
        href = "javascript:void(0)", alt = "ring", title = "ring"
      ),
      tags$area(
        shape = "rect", coords = "1767, 1902, 2100, 1956",
        href = "javascript:void(0)", alt = "trophozoite", title = "trophozoite"
      ),
      tags$area(
        shape = "rect", coords = "2173, 2121, 1946, 2048",
        href = "javascript:void(0)", alt = "schizont", title = "schizont"
      ),
      tags$area(
        shape = "rect", coords = "2122, 2370, 1895, 2234",
        href = "javascript:void(0)", alt = "schizont", title = "schizont"
      ),
      tags$area(
        shape = "rect", coords = "2381, 1810, 3142, 1894",
        href = "javascript:void(0)", alt = "gametocyte", title = "gametocyte"
      ),
      tags$area(
        shape = "rect", coords = "2466, 2179, 3080, 2242",
        href = "javascript:void(0)", alt = "gametocyte", title = "gametocyte"
      ),
      tags$area(
        shape = "rect", coords = "3011, 1298, 3278, 1375",
        href = "javascript:void(0)", alt = "gametocyte", title = "gametocyte"
      ),
      tags$area(
        shape = "rect", coords = "3007, 325, 3424, 413",
        href = "javascript:void(0)", alt = "Oocyst", title = "Oocyst"
      ),
      tags$area(
        shape = "rect", coords = "2016, 790, 2330, 878",
        href = "javascript:void(0)", alt = "sporozoite", title = "sporozoite"
      ),
      tags$area(
        shape = "rect", coords = "779, 1232, 1094, 1305",
        href = "javascript:void(0)", alt = "sporozoite", title = "sporozoite"
      )
    )
  ),

  # Explore data section
  div(
    id = "data_sets_section",
    fluidRow(
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
          tabPanel(
            "Data",
            br(),
            fluidRow(
              column(
                width = 12,
                radioButtons("data_opt", NULL,
                  choices = list(
                    "Metadata" = "meta",
                    "Quality" = "qual",
                    "Sources" = "source"
                  ),
                  selected = "meta"
                )
              )
            ),
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
              )
            ),
            br(),
            fluidRow(withSpinner(
              type = 6,
              size = 0.5,
              color = "#3498db",
              DT::DTOutput("data_table")
            )),
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
            ),
            div(
              id = "source_section",
              fluidRow(
                h3("Table of data sources in here"),
                column(
                  width = 12,
                  actionLink(
                    "toggle_source",
                    HTML("<span id='arrow_source'>&#9656;</span> Data sources")
                  )
                )
              )
            )
          ),
          tabPanel(
            "Gene Exp",
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
                  plotlyOutput("gene_exp_plotly_0"),
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
  )
)
