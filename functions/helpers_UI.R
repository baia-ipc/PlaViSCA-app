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
gene_links <- function(gene_id) {
    plasmo_url <- paste0("https://plasmodb.org/plasmo/app/record/gene/", gene_id)
    ncbi_url <- paste0("https://www.ncbi.nlm.nih.gov/protein/", gene_id)

    shinyjs::runjs(sprintf(
        "
    document.querySelectorAll('[id^=plasmo_link]').forEach(function(el) {
      el.href = '%s';
    });
  ",
        plasmo_url
    ))

    shinyjs::runjs(sprintf(
        "
    document.querySelectorAll('[id^=ncbi_link]').forEach(function(el) {
      el.href = '%s';
    });
  ",
        ncbi_url
    ))
}

# Function that builds the full Metadata tab
create_meta_filter_row <- function(suffix = "") {
    tagList(
        # First row (5 boxes)
        fluidRow(
            class = "meta-filter",
            style = "display: flex; flex-wrap: wrap; justify-content: space-between;",
            column(
                width = 2,
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
            column(
                width = 2,
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
            column(
                width = 2,
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
            column(
                width = 2,
                selectizeInput(
                    inputId = paste0("life_cycle_stage", suffix),
                    "Life cycle stage:",
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
            style = "display: flex; flex-wrap: wrap; justify-content: space-between;",
            column(
                width = 2,
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
            column(
                width = 2,
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
            column(
                width = 2,
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
            column(
                width = 2,
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
            column(
                width = 2,
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
