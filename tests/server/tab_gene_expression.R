check("gene_expression_tab_server adds and removes cards", {
  testServer(function(input, output, session) {
    card_ids <- gene_expression_tab_server(input, output, session, loaded_data(), reactive(study_filter(NULL)))
  }, {
    session$setInputs(tabset = "Gene expression", min_cells_exp = 0, data_type_exp = "normalize",
                      genes_exp_0 = "CSP", group_exp_0 = get_data_column("broad_stage"),
                      check_exp_0 = FALSE, plot_type_exp_0 = "violin")
    session$elapse(1000)
    stop_unless(!is.null(output$gene_exp_plotly_0), "no plot in card 0")
    stop_unless(identical(card_ids(), 0), "card_ids() is not 0")

    session$setInputs(click_add_btn = "add_btn_0")
    stop_unless(identical(card_ids(), c(0, 1)), "card 1 not added")
    session$setInputs(genes_exp_1 = "DBP", group_exp_1 = get_data_column("broad_stage"),
                      check_exp_1 = FALSE, plot_type_exp_1 = "box")
    session$elapse(1000)
    stop_unless(!is.null(output$gene_exp_plotly_1), "no plot in card 1")

    session$setInputs(click_remove_btn = list(btn_id = "remove_btn_1", ids = list(0, 1)))
    stop_unless(identical(card_ids(), 0), "card 1 not removed")
  })
})
