check("gene_card_server draws the plot of a card and writes its download", {
  cells <- gex_cells()
  testServer(function(input, output, session) {
    gene_card_server(3, input, output, reactive(cells), reactiveVal(FALSE))
  }, {
    session$setInputs(genes_exp_3 = "CSP", group_exp_3 = get_data_column("broad_stage"),
                      check_exp_3 = FALSE, plot_type_exp_3 = "violin", min_cells_exp = 0,
                      data_type_exp = "normalize")
    session$elapse(1000)
    stop_unless(!is.null(output$gene_exp_plotly_3), "no plot")
    file <- output$download_gene_exp_3
    sheets <- readxl::excel_sheets(file)
    stop_unless(identical(sheets, c("Expression Data", "Metadata")), paste("sheets:", paste(sheets, collapse = ", ")))
  })
})

check("gene_expression_sheets: expression of the genes and the settings", {
  cells <- gex_cells()
  sheets <- gene_expression_sheets(cells, c("CSP", "DBP"), get_data_column("broad_stage"), FALSE,
                                   "normalize", c("Study" = STUDY))
  stop_unless(nrow(sheets[["Expression Data"]]) == nrow(norm_df), "not one row per cell")
  meta <- sheets[["Metadata"]]
  stop_unless(meta$Value[meta$Parameter == "Gene Name(s)"] == "CSP, DBP", "gene names")
  stop_unless(meta$Value[meta$Parameter == "Study"] == STUDY, "filter value")
  failed <- tryCatch(gene_expression_sheets(cells, character(0), "x", FALSE, "normalize", character(0)),
                     error = function(e) conditionMessage(e))
  stop_unless(identical(failed, "Please select a gene and grouping option"), "no message without genes")
})
