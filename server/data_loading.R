# The expression data of the session: raw or normalized, as chosen by the
# "Data type" control of the tab being viewed.
#
# Returns a list of
#   - gene_data: reactive, list(metadata = mr_data, expression = the
#     expression matrix of the data type (cells x genes, see
#     functions/data/expression.R), genes = gene names and IDs (genes()),
#     type = the data type);
#   - data_type: reactive, "raw" or "normalize";
#   - global_loading: reactiveVal, TRUE while the Gene expression data type
#     is being changed.
data_loading_server <- function(input, session) {
  app_initialized <- reactiveVal(FALSE)
  data_loading_trigger <- reactiveVal(0)
  global_loading <- reactiveVal(FALSE)

  # the data type of the tab being viewed
  data_type <- reactive({
    if (input$tabset == "Gene expression") {
      input$data_type_exp
    } else if (input$tabset == "Embeddings") {
      input$data_type
    } else {
      "normalize"
    }
  })

  # reload 0.3 s after a change of a data type control
  observe({
    list(input$data_type, input$data_type_exp)
    if (!is.null(input$data_type) || !is.null(input$data_type_exp)) {
      if (!app_initialized()) {
        app_initialized(TRUE)
      }
    }
    if (app_initialized()) {
      later(function() isolate(data_loading_trigger(data_loading_trigger() + 1)), 0.3)
    }
  })

  gene_data <- eventReactive(
    list(data_loading_trigger(), input$embedding, input$check_all),
    {
      if (!app_initialized()) {
        return(NULL)
      }
      type <- data_type()
      req(type, input$embedding)

      withProgress(message = "Updating visualization", value = 30, {
        loaded <- expression_data(type)
        list(metadata = mr_data, expression = loaded$expression, genes = loaded$genes, type = type)
      })
    },
    ignoreNULL = FALSE
  )

  sync_data_type_controls(input, session, global_loading)

  list(
    gene_data = gene_data,
    data_type = data_type,
    global_loading = global_loading
  )
}

# The Embeddings and Gene expression tabs each have a "Data type" control
# (data_type, data_type_exp): a change on the tab being viewed is copied to
# the other tab. A change on the Gene expression tab sets `global_loading`
# for 1.5 s, while the data are reloaded.
sync_data_type_controls <- function(input, session, global_loading) {
  observeEvent(input$data_type, {
    if (identical(input$tabset, "Embeddings")) {
      updateRadioButtons(session, "data_type_exp", selected = input$data_type)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$data_type_exp, {
    if (identical(input$tabset, "Gene expression")) {
      global_loading(TRUE)
      updateRadioButtons(session, "data_type", selected = input$data_type_exp)
      later(function() global_loading(FALSE), 1.5)
    }
  }, ignoreInit = TRUE)
}
