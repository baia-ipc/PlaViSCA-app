# Short text summaries of the cells currently selected.

# Helper function to analyze data homogeneity and provide user feedback
analyze_data_diversity <- function(data, group_col) {
    if (is.null(data) || nrow(data) == 0 || !group_col %in% colnames(data)) {
        return(list(
            is_homogeneous = TRUE,
            unique_count = 0,
            message = "No data available for grouping analysis",
            suggestion = "Try different filter combinations"
        ))
    }

    # Remove NA values for analysis
    clean_values <- data[[group_col]][!is.na(data[[group_col]])]
    unique_values <- unique(clean_values)
    unique_count <- length(unique_values)

    # Determine if data is homogeneous (only 1 unique value)
    is_homogeneous <- unique_count <= 1

    if (is_homogeneous) {
        if (unique_count == 0) {
            message <- paste("No valid data found for", group_col)
            suggestion <- "Check your filter settings"
        } else {
            value <- unique_values[1]
            total_cells <- nrow(data)
            message <- paste0(
                "All ", format(total_cells, big.mark = ","), " cells have the same ",
                get_label_for_column(group_col), ": '", value, "'"
            )

            # Provide context-specific suggestions
            if (group_col == get_data_column("detailed_stage")) {
                suggestion <- "This is biologically correct - try grouping by broad life-cycle stage, host species, or tissue/sample type instead"
            } else if (group_col == get_data_column("broad_stage")) {
                suggestion <- "This is expected when filtering by broad life-cycle stage - try the harmonized life-cycle stage or host species grouping"
            } else if (group_col == get_data_column("host_species")) {
                suggestion <- "This filtered dataset contains data from only one host species - try tissue/sample type or study label grouping"
            } else if (group_col == get_data_column("tissue_or_sample_type")) {
                suggestion <- "This filtered dataset contains only one tissue/sample type - try host species or parasite lineage grouping"
            } else {
                suggestion <- "Try different grouping categories or adjust your filters to include more diverse data"
            }
        }
    } else {
        message <- paste0("Found ", unique_count, " different ", get_label_for_column(group_col), " categories")
        suggestion <- ""
    }

    return(list(
        is_homogeneous = is_homogeneous,
        unique_count = unique_count,
        unique_values = if (unique_count <= 10) unique_values else paste0(length(unique_values), " categories"),
        message = message,
        suggestion = suggestion,
        total_cells = nrow(data)
    ))
}

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
