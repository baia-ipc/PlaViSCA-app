# Controls of the Embeddings tab: axes and point size, the gene menus of the
# plots coloured by genes, their "Combine genes" menus and gene links.
# `gene_data` is the reactive from data_loading_server(), `comparison_mode`
# TRUE while the two comparison plots are shown.

embedding_controls_server <- function(input, output, session, gene_data, comparison_mode) {
  # labels of the gene menus: names or IDs
  for (ids in list(c("genes_label_single", "check"), c("genes_label_first_comp", "check_first_comp"),
                   c("genes_label_second", "check_second"))) {
    local({
      check_id <- ids[[2]]
      output[[ids[[1]]]] <- renderText({
        if (isTRUE(input[[check_id]])) "Gene(s) id" else "Gene(s) name"
      })
    })
  }

  # axes of 2D plots: the components of the chosen embedding
  observeEvent(list(input$embedding, input$plot, input$xaxis, input$check_all), {
    if (input$plot == "2d") {
      x_choices <- switch(input$embedding,
        "umap" = if (isTRUE(input$check_all)) umap_i else umap_u,
        "pca" = if (isTRUE(input$check_all)) pca_i else pca_u,
        "tsne" = tsne
      )
      updateSelectInput(session, "xaxis", choices = x_choices, selected = x_choices[1])

      y_choices <- switch(input$embedding,
        "umap" = y_col(input$check_all, input$xaxis, umap_i, umap_u),
        "pca" = y_col(input$check_all, input$xaxis, pca_i, pca_u),
        "tsne" = setdiff(tsne, input$xaxis)
      )
      updateSelectInput(session, "yaxis", choices = y_choices,
                        selected = if (input$yaxis %in% y_choices) input$yaxis else y_choices[2])
    }
  })

  # point size: 2 for 2D plots, 4 for 3D plots
  observeEvent(input$plot,
    {
      size <- switch(input$plot, "2d" = 2, "3d" = 4)
      if (!is.null(size)) {
        updateRadioButtons(session, "size", selected = size)
        runjs(sprintf('$("input[name=\'size\'][value=\'%d\']").prop("checked", true).trigger("change");', size))
      }
    },
    ignoreInit = TRUE
  )

  # gene menus, filled when their plot is coloured by genes; the single plot
  # starts with the first gene
  observeEvent(list(input$color, input$check), {
    if (identical(input$color, "genes")) {
      update_embedding_gene_menu(session, "genes_n", input$genes_n, gene_data()$genes,
                                 isTRUE(input$check), pick_first = TRUE)
    }
  })
  observeEvent(list(input$color_first_comp, input$check_first_comp), {
    if (isTRUE(comparison_mode()) && identical(input$color_first_comp, "genes")) {
      update_embedding_gene_menu(session, "genes_n_first_comp", input$genes_n_first_comp,
                                 gene_data()$genes, isTRUE(input$check_first_comp))
    }
  })
  observeEvent(list(input$color_second, input$check_second), {
    if (isTRUE(comparison_mode()) && identical(input$color_second, "genes")) {
      update_embedding_gene_menu(session, "genes_second", input$genes_second,
                                 gene_data()$genes, isTRUE(input$check_second))
    }
  })

  # the gene menu of the single plot is shown when it is coloured by genes
  observe({
    if (!comparison_mode() && identical(input$color, "genes")) {
      shinyjs::show("genes_single_panel")
    } else {
      shinyjs::hide("genes_single_panel")
    }
  })

  # "Combine genes" menu of each plot: "Each gene separately" is a choice only
  # for 2 or 3 genes. The menu is hidden for fewer than 2 genes and keeps its
  # value (the selection is briefly empty while the name/ID switch converts it).
  for (ids in list(c("genes_n", "combine_genes"), c("genes_n_first_comp", "combine_genes_first_comp"),
                   c("genes_second", "combine_genes_second"))) {
    local({
      genes_id <- ids[[1]]
      combine_id <- ids[[2]]
      observeEvent(length(input[[genes_id]]), {
        n_genes <- length(input[[genes_id]])
        req(n_genes >= 2)
        updateSelectizeInput(session, combine_id, choices = gene_combine_choices(n_genes),
                             selected = effective_gene_combine(input[[combine_id]], n_genes))
      })
    })
  }

  # PlasmoDB and NCBI links of the genes of each plot coloured by genes
  observe({
    req(!isTRUE(comparison_mode()), input$color == "genes", gene_data()$genes)
    gene_links(convert_gene_selection(input$genes_n, gene_data()$genes, TRUE), "plasmo_link")
  })
  observe({
    req(comparison_mode(), input$color_first_comp == "genes", gene_data()$genes)
    gene_links(convert_gene_selection(input$genes_n_first_comp, gene_data()$genes, TRUE), "plasmo_link_a")
  })
  observe({
    req(comparison_mode(), input$color_second == "genes", gene_data()$genes)
    gene_links(convert_gene_selection(input$genes_second, gene_data()$genes, TRUE), "plasmo_link_b")
  })
}

# Fills the gene menu `menu_id` with the genes of `genes_df`, as names or IDs
# (`to_ids`), keeping the selection `selected` (converted to the same kind).
# With `pick_first`, an empty selection becomes the first gene.
update_embedding_gene_menu <- function(session, menu_id, selected, genes_df, to_ids, pick_first = FALSE) {
  if (is.null(genes_df) || nrow(genes_df) == 0) {
    return(invisible(NULL))
  }
  choices <- get_gene_choices(genes_df, to_ids)
  selected <- convert_gene_selection(selected, genes_df, to_ids)
  choices <- c(choices, setdiff(selected, choices))
  if (length(selected) == 0 && pick_first) {
    selected <- choices[1]
  }
  tryCatch(
    updateSelectizeInput(
      session, menu_id,
      choices = choices,
      selected = selected,
      options = list(placeholder = "Select one or more genes", maxItems = 10),
      server = TRUE
    ),
    error = function(e) message("Error updating the gene menu ", menu_id, ": ", conditionMessage(e))
  )
}
