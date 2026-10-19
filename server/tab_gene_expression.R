# Server code of the Gene expression tab: the cells selected by the tab's
# filters (`filters`, a reactive with the filter values, from
# shared_filters_server()), the "Group by" choices, and the cards
# (server/gene_card.R). Card 0 is on the page from the start; the "+" button
# of card i adds card i + 1, the "-" button removes its own card.
# `loaded` is the list returned by data_loading_server(). Returns the IDs of
# the cards on the page (reactive).

gene_expression_tab_server <- function(input, output, session, loaded, filters) {
  gene_data <- loaded$gene_data

  # the cells selected by the filters, and the expression table of the data type
  cells <- reactive({
    req(gene_data())
    tryCatch(
      {
        full_df <- loaded$cached_data[[loaded$data_type()]] %||% gene_data()$data
        meta <- apply_secondary_filters(
          gene_data()$data,
          filters(),
          keep_gene_matrix = FALSE,
          genes = gene_data()$genes$gene
        )
        list(data = meta, genes = gene_data()$genes, full = full_df, row_idx = attr(meta, "row_idx"))
      },
      error = function(e) NULL
    )
  }) %>% debounce(500)

  # IDs of the cards on the page, and the observers of each card
  card_ids <- reactiveVal(0)
  card_observers <- list("0" = gene_card_server(0, input, output, cells, loaded$global_loading))

  # "Group by" choices: the grouping fields present in the data. The default
  # is the detailed stage, then the broad stage, then the first choice.
  observeEvent(cells(),
    {
      fg <- cells()
      if (is.null(fg) || is.null(fg$data)) {
        return()
      }
      valid <- Filter(function(col) col %in% colnames(fg$data), group_choices)
      if (length(valid) == 0) valid <- group_choices

      best_selection <- function(current) {
        if (!is.null(current) && current %in% valid) {
          return(current)
        }
        for (key in c("detailed_stage", "broad_stage")) {
          if (get_data_column(key) %in% valid) {
            return(get_data_column(key))
          }
        }
        valid[1]
      }

      for (i in isolate(card_ids())) {
        group_id <- paste0("group_exp_", i)
        updateSelectInput(session, group_id, choices = valid,
                          selected = best_selection(isolate(input[[group_id]])))
      }
    },
    ignoreInit = TRUE
  )

  # gene menu of card 0, filled once the data are loaded
  observeEvent(gene_data(),
    {
      req(gene_data()$genes)
      choices <- get_gene_choices(gene_data()$genes, FALSE)
      updateSelectizeInput(
        session, "genes_exp_0",
        choices = choices,
        selected = choices[1],
        options = list(placeholder = "Select one or more genes", maxItems = 10),
        server = TRUE
      )
    },
    once = TRUE
  )

  # opening the tab with no gene selected in card 0 selects the first
  observeEvent(input$tabset, {
    if (input$tabset == "Gene expression" && length(input$genes_exp_0) == 0) {
      req(gene_data()$genes)
      choices <- get_gene_choices(gene_data()$genes, input$check_exp_0)
      if (length(choices) > 0) {
        updateSelectizeInput(session, "genes_exp_0", selected = choices[1])
      }
    }
  })

  # "+" button of a card (www/script.js sends click_add_btn): adds a card
  observeEvent(input$click_add_btn, {
    id <- as.numeric(gsub("add_btn_", "", input$click_add_btn))
    if (is.na(id)) {
      return(NULL)
    }
    shinyjs::hide(paste0("add_btn_", id))
    req(cells())

    new_id <- id + 1
    insertUI(
      selector = "#gene_exp_container",
      where = "beforeEnd",
      ui = column(width = 6, id = paste0("dynamic_col_", new_id), gene_expression_card(new_id))
    )
    card_ids(union(card_ids(), new_id))

    choices <- get_gene_choices(cells()$genes, FALSE)
    shinyjs::delay(100, {
      updateSelectizeInput(session, paste0("genes_exp_", new_id),
                           choices = choices, selected = choices[1], server = TRUE)
    })
    card_observers[[as.character(new_id)]] <<- gene_card_server(new_id, input, output, cells, loaded$global_loading)
  })

  # "-" button of a card (www/script.js sends click_remove_btn): removes it
  observeEvent(input$click_remove_btn,
    {
      id <- as.numeric(gsub("remove_btn_", "", input$click_remove_btn$btn_id))
      ids <- as.numeric(unlist(input$click_remove_btn$ids))
      output[[paste0("gene_exp_plotly_", id)]] <- NULL
      output[[paste0("download_gene_exp_", id)]] <- NULL
      removeUI(selector = paste0("#dynamic_col_", id), immediate = TRUE)
      shinyjs::runjs(sprintf("$('#dynamic_col_%s').remove();", id))
      card_ids(setdiff(card_ids(), id))
      card_observers[[as.character(id)]]$destroy()
      card_observers[[as.character(id)]] <<- NULL

      # the "+" button is shown on the last card only
      remaining <- ids[ids != id]
      for (i in remaining) {
        shinyjs::hide(paste0("add_btn_", i))
      }
      shinyjs::show(paste0("add_btn_", if (length(remaining) == 0) 0 else max(remaining)))
    },
    ignoreNULL = TRUE
  )

  # The name/ID switch of a card (www/script.js sends click_toggle_exp): the
  # selected genes are converted to the other kind of identifier; genes
  # without a name keep their ID.
  observeEvent(input$click_toggle_exp, {
    req(gene_data()$genes)
    id <- gsub("check_exp_", "", input$click_toggle_exp$id)
    input_id <- paste0("genes_exp_", id)
    to_ids <- isTRUE(input$click_toggle_exp$value)
    genes_df <- gene_data()$genes
    selected <- convert_gene_selection(isolate(input[[input_id]]), genes_df, to_ids)
    choices <- get_gene_choices(genes_df, to_ids)
    updateSelectizeInput(
      session, input_id,
      label = if (to_ids) "Gene(s) id" else "Gene(s) name",
      choices = c(choices, setdiff(selected, choices)),
      selected = selected,
      server = TRUE
    )
  })

  card_ids
}
