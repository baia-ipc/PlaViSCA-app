# Generate PlasmoDB and NCBI links
gene_links <- function(gene_id, target = "") {
  plasmo_url <- external_link("plasmodb_gene", gene_id)
  ncbi_url <- external_link("ncbi_protein", gene_id)

  plasmo_selector <- if (target == "") "[id^=plasmo_link]" else paste0("#", target)
  ncbi_selector <- if (target == "") "[id^=ncbi_link]" else paste0("#", gsub("plasmo", "ncbi", target))

  shinyjs::runjs(sprintf(
    "
    document.querySelectorAll('%s').forEach(function(el) {
      el.href = '%s';
    });
  ",
    plasmo_selector,
    plasmo_url
  ))

  shinyjs::runjs(sprintf(
    "
    document.querySelectorAll('%s').forEach(function(el) {
      el.href = '%s';
    });
  ",
    ncbi_selector,
    ncbi_url
  ))
}

# Generate links for multiple genes and open multiple tabs
gene_links_multiple <- function(gene_ids, target = "") {
  if (length(gene_ids) == 0) {
    return()
  }

  if (length(gene_ids) == 1) {
    # Single gene - use the regular function
    gene_links(gene_ids[1], target)
  } else {
    # Multiple genes - create JavaScript to open multiple tabs
    plasmo_urls <- external_link("plasmodb_gene", gene_ids)
    ncbi_urls <- external_link("ncbi_protein", gene_ids)

    # Update the first gene's links in the UI
    gene_links(gene_ids[1], target)

    # Create JSON arrays manually for JavaScript
    plasmo_json <- paste0('["', paste(plasmo_urls, collapse = '","'), '"]')
    ncbi_json <- paste0('["', paste(ncbi_urls, collapse = '","'), '"]')

    plasmo_selector <- if (target == "") "[id^=plasmo_link]" else paste0("#", target)
    ncbi_selector <- if (target == "") "[id^=ncbi_link]" else paste0("#", gsub("plasmo", "ncbi", target))

    # Add click handlers for multiple tab opening
    shinyjs::runjs(sprintf(
      "
        document.querySelectorAll('%s').forEach(function(el) {
            el.onclick = function(e) {
                e.preventDefault();
                var urls = %s;
                urls.forEach(function(url) {
                    window.open(url, '_blank');
                });
                return false;
            };
        });
        ",
      plasmo_selector,
      plasmo_json
    ))

    shinyjs::runjs(sprintf(
      "
        document.querySelectorAll('%s').forEach(function(el) {
            el.onclick = function(e) {
                e.preventDefault();
                var urls = %s;
                urls.forEach(function(url) {
                    window.open(url, '_blank');
                });
                return false;
            };
        });
        ",
      ncbi_selector,
      ncbi_json
    ))
  }
}

# One filter widget, built entirely from config/data_columns.yml.
#
# The Shiny input ID is the *logical field key* (e.g. "broad_stage",
# "detailed_stage", "idc_reference_class"), never a physical column name.
create_filter_widget <- function(key, suffix = "", width_class = "col-md-3") {
  help_text <- get_data_help(key)
  opts <- list(
    placeholder = "Select...",
    dropdownParent = "body",
    onInitialize = I('function() { this.setValue(""); }')
  )
  # Fields whose labels are rendered as HTML (e.g. italicised binomials)
  if (get_data_column(key) %in% italicised_columns()) {
    opts$render <- I(
      '{
        option: function(item, escape) { return "<div>" + item.label + "</div>"; },
        item: function(item, escape) { return "<div>" + item.label + "</div>"; }
      }'
    )
  }

  id <- paste0(key, suffix)
  div(
    class = paste(width_class, "filter-widget"),
    # name of the filter, and its "exclude" switch: when on, the selected
    # values are left out instead of kept (functions/filters.R)
    div(
      class = "filter-head",
      tags$label(
        `for` = id,
        class = "control-label",
        tags$span(
          class = "settings-label",
          get_data_label(key),
          if (!is.null(help_text)) {
            tags$span(
              class = "filter-help-icon",
              style = "cursor: help; margin-left: 4px; opacity: 0.65;",
              `data-bs-toggle` = "tooltip",
              `data-bs-placement` = "top",
              title = help_text,
              fontawesome::fa("circle-info", height = "11px")
            )
          }
        )
      ),
      tags$label(
        class = "filter-exclude",
        title = "Leave out the cells with the selected values, instead of keeping them",
        tags$input(type = "checkbox", id = filter_exclude_id(key, suffix)),
        "exclude"
      )
    ),
    selectizeInput(
      inputId = id,
      label = NULL,
      choices = c(""),
      multiple = TRUE,
      options = opts
    )
  )
}

# Settings card (filters, plot settings) whose body folds away when the
# header is clicked. Cards with the same `group` fold and unfold together on
# every tab (www/script.js); `folded` is the starting state. `footer` stays
# visible when the card is folded.
foldable_settings_card <- function(group, icon, title, ..., folded = FALSE,
                                   footer = NULL, class = "mb-2") {
  card(
    class = paste(class, "shadow-sm settings-card-compact foldable-card"),
    # Keep the foldable div a direct child of the card (no extra card body)
    wrapper = NULL,
    card_header(
      class = "bg-primary text-white py-1 foldable-card-header",
      role = "button",
      `data-fold-group` = group,
      `aria-expanded` = tolower(!folded),
      title = "Click to show or hide",
      fontawesome::fa(icon, height = "14px"),
      tags$span(title, class = "fw-bold", style = "font-size: 0.9rem;"),
      # filled in by www/script.js, e.g. "(2)" active filters
      tags$span(class = "foldable-card-count fw-bold", style = "font-size: 0.9rem;"),
      tags$span(class = "foldable-card-chevron", fontawesome::fa("chevron-down", height = "12px"))
    ),
    div(
      class = if (folded) "collapse foldable-card-body" else "collapse show foldable-card-body",
      `data-fold-group` = group,
      ...
    ),
    footer
  )
}

# Builds the shared filter row used by every analysis tab. The set of filters
# and their order come from config/data_columns.yml (`filter_fields:`).
create_meta_filter_row <- function(suffix = "") {
  keys <- filter_field_keys()
  foldable_settings_card(
    group = "filters",
    icon = "filter",
    title = "Filters",
    folded = TRUE,
    # folded, the card becomes a button above the plot settings cog
    # (www/style.css, .filters-folded)
    class = "mb-3 filter-card",
    card_body(
      fluidRow(
        # Five filters per row on wide screens, fewer on narrow ones
        class = "meta-filter g-2 row-cols-1 row-cols-sm-2 row-cols-lg-5",
        lapply(keys, function(k) create_filter_widget(k, suffix, "col")),
      )
    ),
    # Shown only when the broad/detailed life-cycle stage selection is
    # logically impossible (e.g. an old bookmark), instead of silently
    # returning zero rows. Kept outside the foldable body so it is seen
    # even when the filters are folded.
    footer = div(
      id = paste0("stage_compatibility_warning", suffix),
      class = "alert alert-warning py-1 px-2 m-2 small",
      style = "display: none;",
      uiOutput(paste0("stage_compatibility_message", suffix), inline = TRUE)
    )
  )
}

# Plots with their "Plot settings" in a sidebar on the left. The sidebar
# starts folded, showing only a cog button, and unfolds sideways; its open or
# folded state is shared by all tabs (www/script.js). `...` is the main area.
plot_settings_layout <- function(controls, ...) {
  div(
    class = "plot-settings-layout",
    layout_sidebar(
      sidebar = sidebar(
        title = div(
          class = "plot-settings-title",
          fontawesome::fa("cog", height = "14px"), " Plot settings"
        ),
        class = "plot-settings-sidebar",
        width = 240,
        open = "closed",
        controls
      ),
      border = FALSE,
      fillable = FALSE,
      padding = c(0, 0, 0, 12),
      ...
    )
  )
}

# Raw / normalized data choice of a plot settings sidebar.
data_type_control <- function(id) {
  tags$div(
    title = "Select raw or normalized expression data",
    radioButtons(
      id,
      tags$span("Data type", class = "settings-label"),
      choices = list("Raw" = "raw", "Normalized" = "normalize"),
      selected = "normalize",
      inline = TRUE
    )
  )
}

# Generate UI subBlock in gene expression
subBlock <- function(i) {
  card(
    class = "shadow-sm mb-3",
    card_body(
      padding = "10px",
      fluidRow(
        class = "g-1 align-items-end",
        column(
          width = 6,
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
              options = list(maxItems = 10, placeholder = "Search genes...", dropdownParent = "body")
            )
          )
        ),
        column(
          width = 4,
          selectInput(
            paste0("group_exp_", i),
            tags$span("Group by", class = "small fw-semibold"),
            choices = group_choices,
            selected = if (!is.null(group_choices) && length(group_choices) > 0) group_choices[1] else get_data_column("detailed_stage"),
            width = "100%"
          )
        ),
        column(
          width = 2,
          div(
            class = "links-row",
            tags$a(
              id = paste0("plasmo_link_exp_", i),
              href = "#",
              target = "_blank",
              class = "btn btn-outline-info btn-sm gene-link-btn",
              `data-bs-toggle` = "tooltip",
              `data-bs-placement` = "top",
              title = "PlasmoDB",
              tags$i(class = "fas fa-external-link-alt")
            ),
            tags$a(
              id = paste0("ncbi_link_exp_", i),
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
      ),
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

# Create new block helper
newBlock <- function(new_count, counter, choice_exp, dynamicBlock, session) {
  counter(new_count)
  insertUI(
    selector = "#gene_exp_container",
    where = "beforeEnd",
    ui = column(
      width = 6,
      id = paste0("dynamic_col_", new_count),
      dynamicBlock(new_count)
    )
  )

  shinyjs::delay(100, {
    # Add dynamic label rendering for the new block
    observeEvent(input[[paste0("check_exp_", new_count)]], {
      is_checked <- input[[paste0("check_exp_", new_count)]]
      label <- if (is_checked) "Gene(s) id" else "Gene(s) name"
      updateSelectizeInput(session, paste0("genes_exp_", new_count), label = label)
    }, ignoreInit = FALSE)

    updateSelectizeInput(
      session,
      inputId = paste0("genes_exp_", new_count),
      choices = choice_exp,
      server = TRUE
    )
  })
}

# ----------------------------------------------------------------- logos

# Logos of the organizations in config/team.yml, linked to their website
# when one is given. Shown at the right end of the menu bar.
create_org_logos <- function(organizations = team_info()$organizations) {
  logos <- lapply(organizations, function(org) {
    logo <- img(src = org$logo, alt = org$name, class = "logo")
    if (is.null(org$url)) logo else a(href = org$url, target = "_blank", logo)
  })
  div(class = "logo-container", logos)
}
