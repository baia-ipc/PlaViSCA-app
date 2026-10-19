# Short text summaries of the cells currently selected.

# Function to generate data summary for filtered datasets
generate_data_summary <- function(data, filters_applied = NULL) {
    if (is.null(data) || nrow(data) == 0) {
        return("No data available")
    }

    total_cells <- nrow(data)

    # Key metadata columns to summarize (config-resolved, not hardcoded)
    summary_cols <- vapply(
        c(
            "study_label", "host_species", "tissue_or_sample_type",
            "broad_stage", "detailed_stage", "parasite_lineage_isolate",
            "parasite_origin_location", "treatment"
        ),
        get_data_column, character(1)
    )

    available_cols <- intersect(summary_cols, colnames(data))

    summary_parts <- c()
    summary_parts <- c(summary_parts, paste("📊 <b>Total cells:</b>", format(total_cells, big.mark = ",")))

    for (col in available_cols) {
        if (col %in% colnames(data)) {
            unique_vals <- unique(data[[col]][!is.na(data[[col]])])
            count <- length(unique_vals)
            col_display <- get_label_for_column(col)

            if (count == 0) {
                summary_parts <- c(summary_parts, paste("•", col_display, ": No data"))
            } else if (count == 1) {
                summary_parts <- c(summary_parts, paste("•", col_display, ":", unique_vals[1]))
            } else if (count <= 3) {
                summary_parts <- c(summary_parts, paste("•", col_display, "(", count, "):", paste(unique_vals, collapse = ", ")))
            } else {
                summary_parts <- c(summary_parts, paste("•", col_display, ":", count, "different values"))
            }
        }
    }

    if (!is.null(filters_applied) && length(filters_applied) > 0) {
        filter_text <- paste(names(filters_applied), "=", sapply(filters_applied, paste, collapse = ","), collapse = "; ")
        summary_parts <- c(summary_parts, paste("🔍 <b>Filters applied:</b>", filter_text))
    }

    return(paste(summary_parts, collapse = "<br>"))
}
