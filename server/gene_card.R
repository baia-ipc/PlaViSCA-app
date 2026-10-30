# One card of the Gene expression tab (functions/ui/gene_expression_card.R):
# the plot and the "Data" download of card `i`, for the cells `cells`
# (reactive, list(data, genes, expression, row_idx), from
# gene_expression_tab_server()). The inputs of card `i` have the IDs
# <name>_exp_<i>. While `global_loading()` is TRUE the plot says "Loading...".
# Returns the observer of the card's gene links, to destroy with the card.

gene_card_server <- function(i, input, output, cells, global_loading) {
  card_input <- function(name) input[[paste0(name, "_exp_", i)]]

  # the plot waits until the gene and group selections have settled
  genes <- debounce(reactive(card_input("genes")), 800)
  group <- debounce(reactive(card_input("group")), 500)

  output[[paste0("gene_exp_plotly_", i, "_payload")]] <- render_plot_payload(paste0("gene_exp_plotly_", i), {
    selected_genes <- genes()
    selected_group <- group()
    if (length(selected_genes) == 0 || is.null(selected_group)) {
      return(create_empty_plot("Please select at least one gene and a grouping option"))
    }
    if (global_loading()) {
      return(create_empty_plot("Loading..."))
    }

    tryCatch(
      {
        fg <- cells()
        if (is.null(fg) || is.null(fg$data)) {
          return(create_empty_plot("Data not loaded"))
        }
        # the name/ID switch takes effect with the converted genes
        gene_expression_card_plot(
          fg, selected_genes, isolate(card_input("check")), selected_group,
          type = card_input("plot_type"),
          opts = gene_plot_inputs(input, i),
          min_cells = input$min_cells_exp
        )
      },
      error = function(e) create_empty_plot(paste("Error:", e$message))
    )
  })

  output[[paste0("download_gene_exp_", i)]] <- downloadHandler(
    filename = function() gene_expression_file_name(card_input("genes")),
    content = function(file) {
      withProgress(message = "Processing Data", value = 0, {
        incProgress(0.3, detail = "Preparing data...")
        sheets <- tryCatch(
          gene_expression_sheets(
            cells(), card_input("genes"), card_input("group"), card_input("check"),
            data_type = input$data_type_exp,
            filter_values = gene_expression_filter_text(input)
          ),
          error = function(e) {
            list(Error = data.frame(
              Error = "Download failed",
              Message = conditionMessage(e),
              Suggestion = "Please try again or contact support",
              stringsAsFactors = FALSE
            ))
          }
        )
        incProgress(0.7, detail = "Saving file...")
        writexl::write_xlsx(sheets, path = file)
        incProgress(1, detail = "Download complete")
      })
    }
  )

  # PlasmoDB and NCBI buttons: the pages of the selected gene, while only one
  # is selected
  observe({
    req(cells())
    gene_links(convert_gene_selection(card_input("genes"), cells()$genes, TRUE), paste0("plasmo_link_exp_", i))
  })
}

# Options of the plot type of card `i`.
gene_plot_inputs <- function(input, i) {
  opt <- function(name) input[[paste0(name, "_exp_", i)]]
  list(split = opt("split"), combine = opt("combine"), points = opt("points"),
       value = opt("value"), scale = opt("scale"))
}

# Selections of the filters of the Gene expression tab, named by filter label.
gene_expression_filter_text <- function(input) {
  setNames(
    vapply(filter_field_keys(), function(k) paste(input[[paste0(k, "_gex")]], collapse = ", "), character(1)),
    vapply(filter_field_keys(), get_data_label, character(1))
  )
}
