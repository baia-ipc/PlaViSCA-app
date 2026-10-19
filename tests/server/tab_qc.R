check("qc_tab_server counts the cells of the filtered study", {
  testServer(function(input, output, session) {
    qc_tab_server(input, output, reactive(study_filter(STUDY)))
  }, {
    session$flushReact()
    stop_unless(grepl(format(n_cells(STUDY), big.mark = ","), output$total_cells_qc$html, fixed = TRUE),
                "wrong number of cells")
    stop_unless(!is.null(output$data_count_plotly), "no RNA count plot")
    stop_unless(!is.null(output$data_pca_stdv_plotly), "no elbow plot")
  })
})
