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

      # Visualization: a title bar with the plot actions over the single plot
      # or, in comparison mode, the two comparison plots
      card(
        class = "visualization-card shadow-sm",
        card_header(
          class = "bg-primary text-white py-1 visualization-header",
          fontawesome::fa("chart-area", height = "14px"),
          tags$span("Visualization", class = "fw-bold", style = "font-size: 0.9rem;"),
          div(
            class = "visualization-actions",
            # Color by of the single plot (the comparison plots have theirs in
            # their own title bars)
            conditionalPanel(
              condition = "!input.comparison_active",
              div(
                class = "header-color-by",
                tags$span("Color by", class = "header-color-by-label"),
                selectizeInput(
                  "color",
                  NULL,
                  choices = category_choices(),
                  selected = get_data_column("detailed_stage"),
                  width = "190px",
                  options = list(dropdownParent = "body")
                )
              )
            ),
            # Compare Plots only: rotating or zooming one plot moves the other
            # (www/script.js)
            conditionalPanel(
              condition = "input.comparison_active",
              class = "link-views",
              tags$button(
                id = "link_views",
                type = "button",
                class = "btn btn-sm rounded-pill fw-bold linked",
                `aria-pressed` = "true",
                title = "Click to rotate, zoom and pan the two plots together or separately",
                tags$span(class = "when-linked", fontawesome::fa("link"), " Linked"),
                tags$span(class = "when-unlinked", fontawesome::fa("link-slash"), " Unlinked")
              )
            ),
            actionButton(
              "enable_comparison",
              tagList(fontawesome::fa("columns"), " Compare Plots"),
              class = "btn-outline-primary rounded-pill btn-sm fw-bold"
            ),
            downloadButton(
              "download_data", " Download Data",
              class = "btn-light rounded-pill btn-sm fw-bold"
            )
          )
        ),
        card_body(
          fillable = FALSE,
          padding = c(2, 8, 8, 8),

          # Single plot, with its gene selector above it while it is
          # coloured by genes
          div(
            id = "single_plot_panel",
            embedding_gene_panel("input.color == 'genes'", "genes_label_single", "check",
              "genes_n", "plasmo_link", "ncbi_link", "combine_genes",
              btn_class = "btn-outline-info", class = "single-gene-panel mb-2"
            ),
            card(
              full_screen = TRUE,
              height = "650px",
              class = "mb-0",
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
            class = "comparison-plots-container",
            style = "display: none;",
            # the two cards share their rows (title bar, gene selector, plot),
            # so that the plots stay aligned when only one has a gene selector
            div(
              class = "comparison-grid",
              comparison_plot_card("a", "Plot A", "#007bff", "color_first_comp",
                get_data_column("broad_stage"), "first_plotly",
                embedding_gene_panel("input.color_first_comp == 'genes'", "genes_label_first_comp",
                  "check_first_comp", "genes_n_first_comp", "plasmo_link_a", "ncbi_link_a",
                  "combine_genes_first_comp", btn_class = "btn-outline-primary",
                  class = "plot-a-section"
                )
              ),
              comparison_plot_card("b", "Plot B", "#28a745", "color_second",
                get_data_column("detailed_stage"), "second_plotly",
                embedding_gene_panel("input.color_second == 'genes'", "genes_label_second",
                  "check_second", "genes_second", "plasmo_link_b", "ncbi_link_b",
                  "combine_genes_second", btn_class = "btn-outline-success",
                  class = "plot-b-section"
                )
              )
            )
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
      title = "Where the legend goes: over the plot, next to it, or hidden",
      class = "compact-select",
      selectInput(
        "legend_pos",
        tags$span("Legend position", class = "settings-label"),
        choices = LEGEND_POSITIONS,
        selected = "auto",
        width = "130px"
      )
    ),
    tags$div(
      title = "Adjust the size of the legend",
      radioButtons(
        "legend_size",
        tags$span("Legend size", class = "settings-label"),
        choices = names(LEGEND_SIZES),
        selected = "M",
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
    shinyjs::hidden(checkboxInput("comparison_active", NULL, value = FALSE))
  )
}

# Gene selector of an embedding plot, on one row: the gene menu with its
# ID/name switch, the "Combine genes" menu and the PlasmoDB/NCBI links. It is
# shown while `condition` (JavaScript) holds, i.e. while the plot is coloured
# by genes, and sits between the plot and its title bar.
embedding_gene_panel <- function(condition, label_id, check_id, genes_id, plasmo_id, ncbi_id,
                                 combine_id, btn_class, class = NULL) {
  conditionalPanel(
    condition = condition,
    class = paste("embedding-gene-panel", class),
    div(
      class = "embedding-gene-row",
      tags$span(textOutput(label_id, inline = TRUE), class = "header-color-by-label"),
      div(
        class = "embedding-toggle-container",
        title = "Click to switch ID and name",
        `data-bs-toggle` = "tooltip",
        `data-bs-placement` = "top",
        bslib::input_switch(check_id, label = NULL, value = FALSE)
      ),
      div(
        class = "embedding-gene-menu",
        selectizeInput(
          genes_id,
          label = NULL,
          choices = "", multiple = TRUE,
          options = list(placeholder = "Search genes...", maxItems = 10, dropdownParent = "body"),
          width = "100%"
        )
      ),
      gene_combine_menu(combine_id, genes_id),
      div(
        class = "embedding-gene-links",
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
}

# "Combine genes" menu of an embedding plot (choices: COMBINE_GENES), shown
# while more than one gene is selected in `genes_id`; by default "Each gene
# separately", which the server offers only for 2 or 3 genes (otherwise Mean).
gene_combine_menu <- function(id, genes_id) {
  conditionalPanel(
    condition = sprintf("input.%s && input.%s.length > 1", genes_id, genes_id),
    title = sprintf(paste("How the expression of the selected genes gives the colour of a cell",
                          "(Each gene separately: at most %d genes)"), MAX_GENES_EACH_COLOUR),
    div(
      class = "embedding-combine-menu",
      tags$span("Combine genes", class = "header-color-by-label"),
      selectizeInput(
        id,
        NULL,
        choices = COMBINE_GENES,
        selected = "each",
        width = "165px",
        options = list(dropdownParent = "body")
      )
    )
  )
}

# One comparison plot card; its "Color by" selector sits in the title bar,
# `gene_panel` (embedding_gene_panel()) between the title bar and the plot.
comparison_plot_card <- function(side, title, colour, color_id, selected, plot_id, gene_panel) {
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
    card_body(
      div(class = "comparison-gene-slot", gene_panel),
      div(
        class = "comparison-plot-area",
        withSpinner(
          type = 6,
          size = 0.4,
          color = colour,
          plotlyOutput(plot_id, height = "100%")
        )
      )
    )
  )
}
