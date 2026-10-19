check("data_sources_tab_server draws the table of studies", {
  testServer(function(input, output, session) {
    data_sources_tab_server(input, output)
  }, {
    session$setInputs(tabset = "Data sources")
    stop_unless(!is.null(output$data_source), "no table")
  })
})
