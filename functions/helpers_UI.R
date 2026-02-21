# Navigate tabsetpanel
navigate_tabset <- function(session, menu_id, tab_name = NULL) {
  shinyjs::hide("home_section")
  shinyjs::hide("about_section")
  shinyjs::show("analysis_section")

  js_code <- sprintf(
    "
    $('.menu-link').removeClass('active');
    $('#%s').addClass('active');
  ",
    menu_id
  )
  shinyjs::runjs(js_code)

  if (!is.null(tab_name)) {
    updateTabsetPanel(session, inputId = "tabset", selected = tab_name)
  }
}

# Generate PlasmoDB and NCBI links
gene_links <- function(gene_id, target = "") {
  plasmo_url <- paste0("https://plasmodb.org/plasmo/app/record/gene/", gene_id)
  ncbi_url <- paste0("https://www.ncbi.nlm.nih.gov/protein/", gene_id)

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
    plasmo_urls <- paste0("https://plasmodb.org/plasmo/app/record/gene/", gene_ids)
    ncbi_urls <- paste0("https://www.ncbi.nlm.nih.gov/protein/", gene_ids)

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

# Function that builds the full Metadata tab
create_meta_filter_row <- function(suffix = "") {
  card(
    class = "mb-3 shadow-sm",
    card_header(
      class = "bg-primary text-white py-1",
      fontawesome::fa("filter", height = "14px"),
      tags$span("Filters", class = "fw-bold", style = "font-size: 0.9rem;")
    ),
    card_body(
      fluidRow(
        class = "meta-filter g-2", # g-2 for tighter gutters
        div(
          class = "col-md-2",
          selectizeInput(
            inputId = paste0("study_label", suffix),
            label = tags$span("Study label", style = "font-size: 0.85rem; font-weight: 500;"),
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select...",
              dropdownParent = "body",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-md-2",
          selectizeInput(
            inputId = paste0("development_phase", suffix),
            label = tags$span("Development phase", style = "font-size: 0.85rem; font-weight: 500;"),
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select...",
              dropdownParent = "body",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-md-2",
          selectizeInput(
            inputId = paste0("parasite_stages", suffix),
            label = tags$span("Parasite stages", style = "font-size: 0.85rem; font-weight: 500;"),
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select...",
              dropdownParent = "body",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-md-2",
          selectizeInput(
            inputId = paste0("host_species", suffix),
            label = tags$span("Host species", style = "font-size: 0.85rem; font-weight: 500;"),
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select...",
              dropdownParent = "body",
              onInitialize = I('function() { this.setValue(""); }'),
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
          )
        ),
        div(
          class = "col-md-2",
          selectizeInput(
            inputId = paste0("strain", suffix),
            label = tags$span("Strain", style = "font-size: 0.85rem; font-weight: 500;"),
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select...",
              dropdownParent = "body",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-md-2",
          selectizeInput(
            inputId = paste0("hour_post_invasion", suffix),
            label = tags$span("HPI", style = "font-size: 0.85rem; font-weight: 500;"),
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select...",
              dropdownParent = "body",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        )
      )
    )
  )
}

# Create a box for plot controls (data type, point size, dimension)
create_plot_controls <- function(data_type_id = "data_type", include_size_dim = TRUE, suffix = "") {
  size_id <- paste0("size", suffix)
  plot_id <- paste0("plot", suffix)

  card(
    class = "mb-2 shadow-sm settings-card-compact",
    card_header(
      class = "bg-secondary text-white py-1",
      fontawesome::fa("cog", height = "14px"),
      tags$span("Plot settings", class = "fw-bold", style = "font-size: 0.9rem;")
    ),
    card_body(
      layout_column_wrap(
        width = 1/3,
        radioButtons(
          data_type_id,
          tags$span("Data type", class = "settings-label"),
          choices = list("Raw" = "raw", "Normalized" = "normalize"),
          selected = "normalize",
          inline = TRUE
        ),
        if (include_size_dim) {
          tagList(
            radioButtons(
              size_id,
              tags$span("Point size", class = "settings-label"),
              choices = list("S" = 2, "M" = 4, "L" = 6),
              selected = 4,
              inline = TRUE
            ),
            radioButtons(
              plot_id,
              tags$span("Dimension", class = "settings-label"),
              choices = list("2D" = "2d", "3D" = "3d"),
              selected = "3d",
              inline = TRUE
            )
          )
        }
      )
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
            tags$label(
              class = "control-label",
              tags$span(
                title = "Click to switch ID and name",
                `data-bs-toggle` = "tooltip",
                `data-bs-placement` = "top",
                div(
                  class = "gene-expression-toggle-wrapper",
                  input_switch(paste0("check_exp_", i), label = NULL, value = FALSE)
                )
              )
            ),
            selectizeInput(
              paste0("genes_exp_", i),
              label = NULL,
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
            selected = if (!is.null(group_choices) && length(group_choices) > 0) group_choices[1] else "parasite_stages",
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
        actionButton(paste0("remove_btn_", i), label = fontawesome::fa("minus"), class = "btn-sm btn-outline-danger"),
        actionButton(paste0("add_btn_", i), label = fontawesome::fa("plus"), class = "btn-sm btn-outline-success")
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
    updateSelectizeInput(
      session,
      inputId = paste0("genes_exp_", new_count),
      choices = choice_exp,
      server = TRUE
    )
  })
}
