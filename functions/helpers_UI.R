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
  tagList(
    # Filters box container
    div(
      class = "filters-container",
      style = "border: 1px solid #ddd; border-radius: 8px; padding: 20px 20px 10px 20px; margin-bottom: 0; background-color: #f9f9f9;",
      # Filters header
      h4(
        style = "margin-top: 0; margin-bottom: 15px; color: #333; font-weight: 600; border-bottom: 2px solid #3498db; padding-bottom: 10px;",
        icon("filter"),
        " Filters"
      ),
      # First row (5 boxes)
      fluidRow(
        class = "meta-filter",
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("study_label", suffix),
            "Study label:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("sample_type", suffix),
            "Sample type:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("host_species", suffix),
            "Host species:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
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
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("parasite_stage", suffix),
            "Parasite stage:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("treatment", suffix),
            "Treatment:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        )
      ),

      # Second row (5 boxes)
      fluidRow(
        class = "meta-filter",
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("development_phase", suffix),
            "Development phase:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("strain", suffix),
            "Strain:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("hour_post_invasion", suffix),
            "Hour post invasion:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("sequencer", suffix),
            "Sequencer:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(
          class = "col-5-custom",
          selectizeInput(
            inputId = paste0("sc_technology", suffix),
            "Single-cell technology:",
            choices = c(""),
            multiple = TRUE,
            options = list(
              placeholder = "Select options...",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        )
      ) # Close second fluidRow
    ) # Close filters-container div
  )
}

# Create a box for plot controls (data type, point size, dimension)
create_plot_controls <- function(data_type_id = "data_type", include_size_dim = TRUE, suffix = "") {
  size_id <- paste0("size", suffix)
  plot_id <- paste0("plot", suffix)

  fluidRow(
    style = "margin-top: 20px; margin-bottom: 20px;",
    column(
      width = 12,
      div(
        class = "filters-container",
        style = "border: 1px solid #ddd; border-radius: 8px; padding: 20px 20px 10px 20px; background-color: #f9f9f9; margin-bottom: 0;",
        fluidRow(
          column(
            width = if (include_size_dim) 4 else 12,
            radioButtons(
              data_type_id,
              "Data type",
              choices = list("Raw" = "raw", "Normalized" = "normalize"),
              selected = "normalize",
              inline = TRUE
            )
          ),
          if (include_size_dim) {
            tagList(
              column(
                width = 4,
                radioButtons(
                  size_id,
                  "Point size",
                  choices = list("Small" = 2, "Medium" = 4, "Large" = 6),
                  selected = 4,
                  inline = TRUE
                )
              ),
              column(
                width = 4,
                radioButtons(
                  plot_id,
                  "Dimension",
                  choices = list("2D" = "2d", "3D" = "3d"),
                  selected = "3d",
                  inline = TRUE
                )
              )
            )
          }
        )
      )
    )
  )
}

# Generate UI subBlock in gene expression
subBlock <- function(i) {
  tagList(
    fluidRow(
      class = "gene-select-input",
      column(
        width = 6,
        selectizeInput(
          paste0("genes_exp_", i),
          label = "Gene Name",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 4, placeholder = "Select genes...")
        ),
        column(
          width = 2,
          class = "switch_exp",
          tags$label(
            class = "switch",
            tags$input(
              id = paste0("check_exp_", i),
              type = "checkbox",
              class = "toggle_exp"
            ),
            tags$span(class = "slider")
          )
        ),
        column(
          width = 4,
          id = paste0("gene_link_exp_", i),
          class = "gene_link_exp",
          tags$div(
            id = paste0("gene_link_ui_exp_", i),
            class = "gene_link_ui_exp",
            tags$a(
              id = paste0("plasmo_link_exp_", i),
              href = "#",
              target = "_blank",
              "🔬 PlasmoDB"
            ),
            span("|"),
            tags$a(
              id = paste0("ncbi_link_exp_", i),
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
          paste0("group_exp_", i),
          "Group by",
          choices = group_choices,
          selected = if (!is.null(group_choices) && length(group_choices) > 0) group_choices[1] else "life_cycle_stage"
        )
      )
    ),
    fluidRow(
      width = 12,
      fluidRow(withSpinner(plotlyOutput(paste0("gene_exp_plotly_", i)), type = 6, size = 0.5, color = "#3498db")),
      fluidRow(
        class = "btn-add-rm-container",
        downloadButton(
          paste0("download_gene_exp_", i),
          label = "",
          class = "btn btn-secondary btn_add"
        ),
        actionButton(
          paste0("remove_btn_", i),
          label = icon("minus"),
          class = "btn btn-danger btn_remove"
        ),
        actionButton(
          paste0("add_btn_", i),
          label = icon("plus"),
          class = "btn btn-success btn_add"
        )
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
