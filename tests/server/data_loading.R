check("data_loading_server loads the normalized data of the Embeddings tab", {
  testServer(function(input, output, session) {
    loaded <- data_loading_server(input, session)
  }, {
    session$setInputs(tabset = "Embeddings", data_type = "normalize", embedding = "umap", check_all = TRUE)
    wait_later(0.4)
    session$flushReact()
    gd <- loaded$gene_data()
    stop_unless(identical(gd$expression, NORMALIZED$expression), "gene_data() is not the normalized matrix")
    stop_unless(identical(gd$metadata, mr_data), "gene_data() does not carry the metadata")
    stop_unless(nrow(gd$genes) > 1000, "no gene list")
    stop_unless(identical(gd$genes$gene, expression_gene_ids(gd$expression)), "the gene list is not the matrix columns")
    stop_unless(identical(loaded$data_type(), "normalize"), "data_type() is not normalize")
  })
})

check("changing the Gene expression data type sets global_loading", {
  testServer(function(input, output, session) {
    loaded <- data_loading_server(input, session)
  }, {
    session$setInputs(tabset = "Gene expression", data_type = "normalize", data_type_exp = "normalize")
    session$setInputs(data_type_exp = "raw")
    stop_unless(isTRUE(loaded$global_loading()), "global_loading() is not set")
    stop_unless(identical(loaded$data_type(), "raw"), "data_type() is not the Gene expression data type")
  })
})
