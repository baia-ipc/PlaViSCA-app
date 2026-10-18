# Every field in config/data_columns.yml maps to a column of the loaded data.

test_that_("every configured field maps to a column of the metadata", {
    m <- app_data("mr_data")
    unresolved <- character(0)
    for (key in data_field_keys()) {
        col <- get_data_column(key)
        if (is.null(col) || !nzchar(col)) {
            unresolved <- c(unresolved, sprintf("%s (no column)", key))
        } else if (!col %in% colnames(m)) {
            unresolved <- c(unresolved, sprintf("%s -> %s", key, col))
        }
    }
    expect_empty_(unresolved, "configured fields with no column in the metadata")
})

test_that_("every configured field also exists in the expression tables", {
    n <- app_data("norm_df")
    missing <- character(0)
    for (key in data_field_keys()) {
        col <- get_data_column(key)
        if (!col %in% colnames(n)) missing <- c(missing, sprintf("%s -> %s", key, col))
    }
    expect_empty_(missing, "configured fields absent from normalize_df")
})

test_that_("every field referenced by the filter/category/detail/plot lists is configured", {
    known <- data_field_keys()
    unknown <- setdiff(
        unique(c(filter_field_keys(), category_field_keys(), detail_table_field_keys(), plot_field_keys())),
        known
    )
    expect_empty_(unknown, "field keys used by the UI but not declared in the config")
})
