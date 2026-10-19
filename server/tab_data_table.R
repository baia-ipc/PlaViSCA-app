# Server code of the Metadata table tab: one row per cell selected by the
# tab's filters (`filters`, a reactive with the filter values, from
# shared_filters_server()). The columns, their order and labels come from
# config/data_columns.yml (`detail_table_fields:`); true NA is shown under the
# field's missing label, the data are not changed.

data_table_tab_server <- function(output, filters) {
  observeEvent(filters(), {
    filt_data <- apply_secondary_filters(mr_data, filters())

    output$data_table <- renderDT(
      {
        detail_keys <- Filter(
          function(k) get_data_column(k) %in% colnames(filt_data),
          detail_table_field_keys()
        )
        detail_cols <- vapply(detail_keys, get_data_column, character(1))

        processed_data <- filt_data[, detail_cols, drop = FALSE]
        for (k in detail_keys) {
          col <- get_data_column(k)
          v <- processed_data[[col]]
          if (is.character(v) || is.factor(v) || is.logical(v)) {
            processed_data[[col]] <- relabel_missing(display_values(col, v), missing_label_for(col))
          }
        }
        names(processed_data) <- vapply(detail_keys, get_data_label, character(1))

        sort_by <- get_data_label("study_label")
        count_by <- get_data_label("n_count_rna")
        processed_data <- processed_data[
          order(processed_data[[sort_by]], -processed_data[[count_by]]), ,
          drop = FALSE
        ]

        DT::datatable(
          processed_data,
          escape = FALSE,
          extensions = "Buttons",
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            # page length and search on one row, as in Data sources
            dom = "<'d-flex justify-content-between align-items-center'lf>rt<'d-flex justify-content-between align-items-center'ip>B",
            buttons = list(
              list(
                extend = "csv",
                text = "Export .csv",
                className = "btn btn-success"
              )
            )
          ),
          selection = "single",
          filter = "top",
          rownames = FALSE
        )
      },
      server = TRUE
    )
  })
}
