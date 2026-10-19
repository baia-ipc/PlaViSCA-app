# A card of the Gene expression tab: plot type, genes, grouping, the options
# of the plot type (GENE_PLOT_OPTIONS in functions/plots/gene_expression.R),
# the plot and its buttons. Card `i` has the input IDs <name>_exp_<i>.

# Menus open on top of the page, not inside the card, so that the next row of
# menus does not cover them.
DROPDOWN_ON_BODY <- list(dropdownParent = "body")

gene_expression_card <- function(i) {
  card(
    class = "shadow-sm mb-3",
    card_body(
      padding = "10px",
      fluidRow(
        class = "g-1 align-items-end",
        column(
          width = 4,
          selectizeInput(
            paste0("plot_type_exp_", i),
            tags$span("Plot type", class = "small fw-semibold"),
            choices = GENE_PLOT_TYPES,
            width = "100%",
            options = DROPDOWN_ON_BODY
          )
        ),
        column(width = 6, gene_select(i)),
        column(width = 2, gene_link_buttons(i))
      ),
      gene_plot_options_row(i),
      withSpinner(plotlyOutput(paste0("gene_exp_plotly_", i), height = "400px"), type = 6, size = 0.5, color = "#3498db"),
      div(
        class = "d-flex justify-content-end gap-2 mt-2",
        downloadButton(paste0("download_gene_exp_", i), label = "Data", class = "btn-sm btn-outline-secondary"),
        actionButton(paste0("remove_btn_", i), label = fontawesome::fa("minus"), class = "btn-sm btn-outline-danger btn_remove"),
        actionButton(paste0("add_btn_", i), label = fontawesome::fa("plus"), class = "btn-sm btn-outline-success btn_add")
      )
    )
  )
}

# Gene selection, with the switch between gene names and IDs.
gene_select <- function(i) {
  div(
    class = "gene-select-wrapper",
    style = "position: relative;",
    div(
      class = "gene-expression-toggle-container",
      title = "Click to switch ID and name",
      `data-bs-toggle` = "tooltip",
      `data-bs-placement` = "top",
      input_switch(paste0("check_exp_", i), label = NULL, value = FALSE)
    ),
    selectizeInput(
      paste0("genes_exp_", i),
      label = "Gene(s) name",
      choices = NULL,
      multiple = TRUE,
      options = c(list(maxItems = 10, placeholder = "Search genes..."), DROPDOWN_ON_BODY)
    )
  )
}

# PlasmoDB and NCBI buttons of the selected gene(s).
gene_link_buttons <- function(i) {
  link <- function(id, title, icon) {
    tags$a(
      id = paste0(id, i), href = "#", target = "_blank",
      class = "btn btn-outline-info btn-sm gene-link-btn",
      `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
      title = title, tags$i(class = icon)
    )
  }
  div(
    class = "links-row",
    link("plasmo_link_exp_", "PlasmoDB", "fas fa-external-link-alt"),
    link("ncbi_link_exp_", "NCBI", "fas fa-dna")
  )
}

# Group by, and the options of the plot type, each shown only for the plot
# types that use it.
gene_plot_options_row <- function(i) {
  field <- function(name, label, choices, selected) {
    selectizeInput(paste0(name, "_exp_", i), tags$span(label, class = "small fw-semibold"),
                   choices = choices, selected = selected, width = "100%",
                   options = DROPDOWN_ON_BODY)
  }
  split_choices <- c("None" = NO_SPLIT, group_choices)
  fluidRow(
    class = "g-1 align-items-end",
    column(
      width = 4,
      field("group", "Group by", group_choices,
            if (length(group_choices) > 0) group_choices[1] else get_data_column("detailed_stage"))
    ),
    column(
      width = 4,
      gene_plot_option(i, "split", field("split", "Split by", split_choices, NO_SPLIT)),
      gene_plot_option(i, "value", field("value", "Value", SUMMARY_VALUES, "percent"))
    ),
    column(
      width = 4,
      gene_plot_option(i, "combine", field("combine", "Combine genes", COMBINE_GENES, "mean")),
      gene_plot_option(i, "points", field("points", "Points by", split_choices,
                                          get_data_column("study_label"))),
      gene_plot_option(i, "scale", div(
        class = "pb-2",
        title = "Scale each gene to mean 0 and standard deviation 1 across the groups",
        checkboxInput(paste0("scale_exp_", i), "Scale per gene", value = TRUE)
      ))
    )
  )
}

# `ui`, shown only while the plot type of card `i` uses `option`.
gene_plot_option <- function(i, option, ui) {
  types <- names(Filter(function(opts) option %in% opts, GENE_PLOT_OPTIONS))
  condition <- sprintf(
    "[%s].indexOf(input['plot_type_exp_%s']) >= 0",
    paste0("'", types, "'", collapse = ", "), i
  )
  conditionalPanel(condition, ui)
}
