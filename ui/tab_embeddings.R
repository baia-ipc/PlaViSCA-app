# Embeddings tab: PCA / UMAP / t-SNE plots with filters, colouring and comparison mode.

ui_tab_embeddings <- function() {
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
  )
}
