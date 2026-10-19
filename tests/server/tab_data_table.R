check("data_table_tab_server shows the cells of the filtered study", {
  testServer(function(input, output, session) {
    data_table_tab_server(output, reactive(study_filter(STUDY)))
  }, {
    session$flushReact()
    stop_unless(!is.null(output$data_table), "no table")
  })
})
