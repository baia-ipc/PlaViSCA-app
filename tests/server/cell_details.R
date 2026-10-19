check("cell_row and cell_expression_table: the expressed genes of a cell", {
  cells <- apply_secondary_filters(norm_df, study_filter(STUDY), keep_gene_matrix = FALSE,
                                   genes = genes(norm_df, gff_data)$gene)
  row <- cell_row("5", cells, norm_df)
  stop_unless(identical(row, attr(cells, "row_idx")[5]), "position key not mapped to its row")
  stop_unless(identical(cell_row(norm_df$barcode[row], cells, norm_df), row), "barcode key not mapped")
  stop_unless(is.na(cell_row("no such cell", cells, norm_df)), "unknown cell found")
  tbl <- cell_expression_table(row, norm_df, gff_data)
  stop_unless(nrow(tbl) > 0 && all(tbl$exp_value > 0) && !is.unsorted(rev(tbl$exp_value)),
              "not the expressed genes, most expressed first")
  stop_unless(identical(names(tbl), c("gene_id", "gene_name", "exp_value", "description")), "columns")
  stop_unless(nrow(cell_expression_table(NA, norm_df, gff_data)) == 0, "a table for no cell")
})

check("cell_details_server opens the table of a cell", {
  cells <- apply_secondary_filters(norm_df, study_filter(STUDY), keep_gene_matrix = FALSE,
                                   genes = genes(norm_df, gff_data)$gene)
  testServer(function(input, output, session) {
    table <- cell_details_server(input, output, reactive(list(data = cells)),
                                 reactive(list(data = norm_df)), reactiveVal(TRUE))
  }, {
    session$setInputs(embedding_cell_open = list(key = "5", at = 1))
    stop_unless(nrow(table()) > 0, "no table")
    stop_unless(file.exists(output$export_data), "no export")
  })
})
