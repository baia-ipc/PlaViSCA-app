check("cell_row and cell_expression_table: the expressed genes of a cell", {
  expression <- NORMALIZED$expression
  cells <- apply_secondary_filters(mr_data, study_filter(STUDY))
  row <- cell_row("5", cells, expression)
  stop_unless(identical(row, attr(cells, "row_idx")[5]), "position key not mapped to its row")
  stop_unless(identical(cell_row(rownames(expression)[row], cells, expression), row), "barcode key not mapped")
  stop_unless(is.na(cell_row("no such cell", cells, expression)), "unknown cell found")
  tbl <- cell_expression_table(row, expression, gff_data)
  stop_unless(nrow(tbl) > 0 && all(tbl$exp_value > 0) && !is.unsorted(rev(tbl$exp_value)),
              "not the expressed genes, most expressed first")
  stop_unless(nrow(tbl) == sum(expression[row, ] > 0), "not every expressed gene of the cell")
  stop_unless(identical(names(tbl), c("gene_id", "gene_name", "exp_value", "description")), "columns")
  stop_unless(nrow(cell_expression_table(NA, expression, gff_data)) == 0, "a table for no cell")
})

check("cell_details_server opens the table of a cell", {
  cells <- apply_secondary_filters(mr_data, study_filter(STUDY))
  testServer(function(input, output, session) {
    plot_cells <- reactiveValues(all_plotly = list(1:3, 4:nrow(cells)))
    table <- cell_details_server(input, output, reactive(list(data = cells)),
                                 reactive(list(expression = NORMALIZED$expression)), reactiveVal(TRUE),
                                 plot_cells)
  }, {
    session$setInputs(embedding_cell_open = list(plot = "all_plotly", curve = 1, point = 1, at = 1))
    stop_unless(nrow(table()) > 0, "no table")
    row <- cell_row(5L, cells, NORMALIZED$expression)
    stop_unless(nrow(table()) == sum(NORMALIZED$expression[row, ] > 0), "not the genes of the 5th cell")
    stop_unless(file.exists(output$export_data), "no export")
  })
})
