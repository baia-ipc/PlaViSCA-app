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
              img(src = "umap.png", alt = "Analysis"),
              span("Analysis")
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
        column(
          width = 12,
          class = "col-12",
          div(
            class = "form-group shiny-input-container",
            id = "apply_filter",
            tags$label("Data filters:", `for` = "apply_filter"),
            div(
              id = "apply_filter_wrapper",
              class = "dropdown-container",
              selectizeInput(
                "apply_filter",
                label = NULL,
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "No filter applied",
                  render = I(
                    '{
                      option: function(item, escape) {
                        return "<div>" + item.label + "</div>";
                      },
                      item: function(item, escape) {
                        return "<div>" + item.label + "</div>";
                      }
                    }'
                  )
                )
              ),
              tags$div(class = "dropdown-arrow", title = "Open dropdown", "▼")
            )
          )
        ),
        column(
          width = 12,
          class = "col-12",
          radioButtons(
            "color",
            "Color",
            choices = list(
              "Study" = "study_label",
              "Life cycle" = "life_cycle_stage",
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
        column(
          width = 12,
          class = "col-12",
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
          ),
          fluidRow(
            id = "gene_snn_ui",
            column(
              width = 7,
              selectizeInput("genes_n", "Gene", choices = "")
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
                class = "col-12",
                tags$a(
                  id = "plasmo_link",
                  href = "#",
                  target = "_blank",
                  "🔬 PlasmoDB"
                ),
                tags$a(
                  id = "ncbi_link",
                  href = "#",
                  target = "_blank",
                  "🧬 NCBI"
                )
              )
            )
          )
        ),
        column(
          width = 12,
          class = "col-12",
          radioButtons(
            "size",
            "Size",
            choices = list("S" = 2, "M" = 4, "L" = 6),
            selected = 4
          )
        ),
        column(
          width = 12,
          class = "col-12",
          radioButtons(
            "plot",
            "Plot",
            choices = list("2D" = "2d", "3D" = "3d"),
            selected = "3d"
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
            fluidRow(
              column(
                id = "tab_switch",
                width = 5,
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
              ),
              column(
                width = 6,
                class = "radio",
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
            ),
            fluidRow(
              withSpinner(
                type = 6,
                size = 0.5,
                color = "#3498db",
                plotlyOutput("all_plotly")
              )
            )
          ),

          # gene expression section (moved to second position)
          tabPanel(
            "Gene expression",
            fluidRow(
              column(
                width = 12,
                create_meta_filter_row("_exp")
              )
            ),
            fluidRow(
              column(
                width = 12,
                radioButtons(
                  "data_type_exp",
                  "Data type",
                  choices = list(
                    "Raw" = "raw",
                    "Normalized" = "normalize"
                    # "Scaled" = "scale"
                  ),
                  selected = "normalize"
                )
              )
            ),
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
                                     options = list(maxItems = 4, placeholder = "Select genes...")),
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
                            "🔬 PlasmoDB"
                          ),
                          span("|"),
                          tags$a(
                            id = "ncbi_link_exp",
                            href = "#",
                            target = "_blank",
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
                        selected = "life_cycle_stage"
                      )
                    )
                  ),
                  # Data summary information panel
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
            div(
              id = "qual_section",
              create_meta_filter_row("_qc"),
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
            div(
              id = "meta_section",
              create_meta_filter_row(),
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
                      "<em>Note: All life cycle stages are predicted, except for the sporozoite.</em>"
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
