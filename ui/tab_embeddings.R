# Embeddings tab: PCA / UMAP / t-SNE plots with filters, colouring and comparison mode.

ui_tab_embeddings <- function() {
  tabPanel(
    tagList(fontawesome::fa("diagram-project"), " Embeddings"),
    value = "Embeddings",
    # Shared filter row
    create_meta_filter_row(""),

    # Plot settings in a sidebar left of the plots
    plot_settings_layout(
      embedding_settings_controls(),

      # Single plot
      div(
        id = "single_plot_panel",
        card(
          full_screen = TRUE,
          height = "650px",
          card_header("Visualization"),
          withSpinner(
            type = 6,
            size = 0.5,
            color = "#3498db",
            plotlyOutput("all_plotly", height = "100%")
          )
        )
      ),

      # Gene selectors for the comparison plots, shown above each plot only
      # when that plot is coloured by genes. The "Color by" selectors
      # themselves sit in the plot title bars.
      div(
        id = "comparison_controls_panel",
        style = "display: none;",
        layout_column_wrap(
          width = 1 / 2,
          comparison_gene_panel("a", "color_first_comp", "genes_label_first_comp",
            "check_first_comp", "genes_n_first_comp", "plasmo_link_a", "ncbi_link_a",
            btn_class = "btn-outline-primary"
          ),
          comparison_gene_panel("b", "color_second", "genes_label_second",
            "check_second", "genes_second", "plasmo_link_b", "ncbi_link_b",
            btn_class = "btn-outline-success"
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
          height = "600px",
          heights_equal = "all",
          comparison_plot_card("a", "Plot A", "#007bff", "color_first_comp",
            get_data_column("broad_stage"), "first_plotly"
          ),
          comparison_plot_card("b", "Plot B", "#28a745", "color_second",
            get_data_column("detailed_stage"), "second_plotly"
          )
        )
      )
    )
  )
}

# Controls of the plot settings sidebar, one below the other.
embedding_settings_controls <- function() {
  tagList(
    data_type_control("data_type"),
    conditionalPanel(
      condition = "input.embedding != 'tsne'",
      div(
        class = "form-group shiny-input-container mb-0",
        tags$label(
          class = "control-label",
          `for` = "check_all",
          tags$span("Integration", class = "settings-label")
        ),
        div(
          id = "tab_switch",
          class = "switch-container-compact d-flex align-items-center gap-1",
          tags$span("Unintegrated", class = "switch-text text-muted switch-text-small"),
          input_switch("check_all", label = NULL, value = TRUE),
          tags$span("Integrated", class = "switch-text switch-text-small")
        )
      )
    ),
    tags$div(
      title = "Choose the dimensionality reduction method",
      radioButtons(
        "embedding",
        tags$span("Embedding", class = "settings-label"),
        inline = TRUE,
        choices = list("PCA" = "pca", "UMAP" = "umap", "tSNE" = "tsne"),
        selected = "umap"
      )
    ),
    tags$div(
      title = "Adjust the size of the points in the plot",
      radioButtons(
        "size",
        tags$span("Point size", class = "settings-label"),
        choices = list("S" = 2, "M" = 4, "L" = 6),
        selected = 4,
        inline = TRUE
      )
    ),
    tags$div(
      title = "Select 2D or 3D visualization",
      radioButtons(
        "plot",
        tags$span("Dimension", class = "settings-label"),
        choices = list("2D" = "2d", "3D" = "3d"),
        selected = "3d",
        inline = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.plot == '2d'",
      div(
        class = "xy-selector-horizontal",
        div(class = "d-flex align-items-center gap-1", tags$span("X", class = "xy-label"), tags$div(selectInput("xaxis", NULL, choices = c("pc_1" = "pc_1"), selected = "pc_1", width = "80px"))),
        div(class = "d-flex align-items-center gap-1", tags$span("Y", class = "xy-label"), tags$div(selectInput("yaxis", NULL, choices = c("pc_2" = "pc_2"), selected = "pc_2", width = "80px")))
      )
    ),
    shinyjs::hidden(checkboxInput("comparison_active", NULL, value = FALSE)),
    # Color by (single plot only; comparison plots have theirs in the title bars)
    conditionalPanel(
      condition = "!input.comparison_active",
      selectizeInput(
        "color",
        tags$span("Color by", class = "settings-label"),
        choices = category_choices(),
        selected = get_data_column("broad_stage"),
        width = "100%",
        options = list(dropdownParent = "body")
      )
    ),
    # Gene selection, shown when the single plot is coloured by genes
    div(
      id = "genes_single_panel",
      style = "display: none;",
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
      ),
      div(
        class = "links-row mt-1",
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
    ),
    div(
      class = "d-grid gap-2 mt-2",
      actionButton(
        "enable_comparison",
        tagList(fontawesome::fa("columns"), " Compare Plots"),
        class = "btn-outline-primary shadow-sm rounded-pill btn-sm fw-bold"
      ),
      downloadButton(
        "download_data", " Download Data",
        class = "btn-success shadow-sm rounded-pill btn-sm fw-bold"
      )
    )
  )
}

# Gene selector (with ID/name switch and PlasmoDB/NCBI links) for one
# comparison plot; visible only while that plot is coloured by genes.
comparison_gene_panel <- function(side, color_id, label_id, check_id, genes_id,
                                  plasmo_id, ncbi_id, btn_class) {
  conditionalPanel(
    condition = sprintf("input.%s == 'genes'", color_id),
    class = sprintf("mt-2 comparison-controls comparison-plot-section plot-%s-section", side),
    fluidRow(
      class = "g-1 align-items-end",
      column(
        width = 10,
        div(
          class = "gene-select-wrapper",
          style = "position: relative;",
          tags$label(
            class = "control-label d-flex align-items-center gap-2",
            tags$span(textOutput(label_id, inline = TRUE), class = "settings-label"),
            div(
              class = sprintf("comparison-toggle-container-%s", side),
              title = "Click to switch ID and name",
              `data-bs-toggle` = "tooltip",
              `data-bs-placement` = "top",
              bslib::input_switch(check_id, label = NULL, value = FALSE)
            )
          ),
          selectizeInput(
            genes_id,
            label = NULL,
            choices = "", multiple = TRUE,
            options = list(placeholder = "Search genes...", maxItems = 10, dropdownParent = "body"),
            width = "100%"
          )
        )
      ),
      column(
        width = 2,
        div(
          class = "links-row",
          tags$a(
            id = plasmo_id, href = "#", target = "_blank",
            class = paste("btn btn-sm gene-link-btn", btn_class),
            `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
            title = "PlasmoDB",
            tags$i(class = "fas fa-external-link-alt")
          ),
          tags$a(
            id = ncbi_id, href = "#", target = "_blank",
            class = paste("btn btn-sm gene-link-btn", btn_class),
            `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
            title = "NCBI",
            tags$i(class = "fas fa-dna")
          )
        )
      )
    )
  )
}

# One comparison plot card; its "Color by" selector sits in the title bar.
comparison_plot_card <- function(side, title, colour, color_id, selected, plot_id) {
  card(
    full_screen = TRUE,
    class = sprintf("comparison-card plot-%s-card", side),
    card_header(
      class = "comparison-card-header py-1",
      div(
        class = "d-flex align-items-center gap-2",
        fontawesome::fa("circle", fill = colour, height = "12px"),
        tags$span(title, class = "fw-semibold small"),
        div(
          class = "ms-auto d-flex align-items-center gap-1 header-color-by",
          tags$span("Color by", class = "header-color-by-label"),
          selectizeInput(
            color_id,
            NULL,
            choices = category_choices(),
            selected = selected,
            width = "170px",
            options = list(dropdownParent = "body")
          )
        )
      )
    ),
    withSpinner(
      type = 6,
      size = 0.4,
      color = colour,
      plotlyOutput(plot_id, height = "100%")
    )
  )
}
