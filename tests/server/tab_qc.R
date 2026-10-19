check("qc_tab_server counts the cells of the filtered study", {
  testServer(function(input, output, session) {
    qc_tab_server(output, reactive(study_filter(STUDY)))
  }, {
    session$flushReact()
    stop_unless(grepl(format(n_cells(STUDY), big.mark = ","), output$total_cells_qc$html, fixed = TRUE),
                "wrong number of cells")
    stop_unless(!is.null(output$data_count_plotly), "no RNA count plot")
    stop_unless(!is.null(output$data_pca_stdv_plotly), "no elbow plot")
  })
})

check("the elbow plot of the QC tab is of the study its filters select", {
  stop_unless(identical(qc_study(study_filter(STUDY)), STUDY), "not the selected study")
  excluded <- study_filter(unique(mr_data$study_label)[1])
  attr(excluded, "exclude") <- get_data_column("study_label")
  stop_unless(identical(qc_study(excluded), unique(mr_data$study_label)[2]), "excluded study shown")
  stop_unless(identical(qc_study(study_filter(NULL)), unique(mr_data$study_label)[1]), "no default study")
})
