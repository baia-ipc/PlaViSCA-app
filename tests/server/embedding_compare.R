check("embedding_compare_server switches to two plots with the colour of the single plot", {
  drawn <- character(0)
  testServer(function(input, output, session) {
    comparison_mode <- reactiveVal(FALSE)
    embedding_compare_server(input, output, session, reactive(NULL), comparison_mode,
                             function(params, dual) {
                               drawn <<- c(drawn, params$color)
                               plotly::plotly_empty()
                             })
  }, {
    session$setInputs(color = "study_label", plot = "3d", size = 4)
    session$setInputs(enable_comparison = 1)
    stop_unless(isTRUE(comparison_mode()), "comparison mode not on")
    session$setInputs(color_first_comp = "study_label", color_second = get_data_column("detailed_stage"))
    stop_unless(!is.null(output$first_plotly) && !is.null(output$second_plotly), "no plots")
    stop_unless(all(c("study_label", get_data_column("detailed_stage")) %in% drawn), "plots not drawn with their colours")
    session$setInputs(enable_comparison = 2)
    stop_unless(!isTRUE(comparison_mode()), "comparison mode not off")
  })
})
