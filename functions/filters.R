# Filtering cells by the shared filter widgets.
# =============================================================================
# Reads the filter selections of one tab and applies them to the metadata or
# expression table. Field names come from config/data_columns.yml.
# =============================================================================

# Standard set of metadata filter fields shared by the Embeddings, Metadata,
# QC and Gene expression tabs. The list itself lives in
# config/data_columns.yml (`filter_fields:`) so there is exactly one
# authoritative vocabulary - do not re-declare field names here.
FILTER_FIELDS <- filter_field_keys()

# Order numeric filter values by magnitude while preserving their original type.
sort_numeric_choices <- function(values) {
    values <- unique(values)
    numeric_values <- suppressWarnings(as.numeric(as.character(values)))

    values[order(is.na(numeric_values), numeric_values, as.character(values), na.last = TRUE)]
}

# Collects the current filter widget selections into a named list keyed by the
# actual data column names (via get_data_column()), suitable for
# apply_secondary_filters()/fun_filter_df(). `suffix` selects which tab's set
# of widgets to read (e.g. "_meta", "_table", "_qc", "_gex", or "" for Embeddings).
collect_filter_inputs <- function(input, suffix = "") {
    setNames(
        lapply(FILTER_FIELDS, function(field) input[[paste0(field, suffix)]]),
        vapply(FILTER_FIELDS, get_data_column, character(1))
    )
}

# Helper function to format species names with italics.
#
# Anything that looks like a binomial ("Genus species") is italicised and the
# missing label of the column is left alone, so a newly added host species is
# italicised without editing a list of species.
format_species_name <- function(species_names, column = NULL) {
    missing_label <- missing_label_for(column)
    chr <- relabel_missing(species_names, missing_label)
    is_binomial <- grepl("^[A-Z][a-z]+ [a-z]+", chr) & chr != missing_label
    ifelse(is_binomial, paste0("<i>", chr, "</i>"), chr)
}

# The set of metadata (non-gene) columns the app works with, resolved from
# config/data_columns.yml. Used to separate metadata from the gene matrix.
app_metadata_columns <- function() {
    unique(vapply(data_field_keys(), get_data_column, character(1), USE.NAMES = FALSE))
}

fun_filter_df <- function(data, selections) {
    if (is.null(data)) {
        return(NULL)
    }
    if (is.null(selections) || length(selections) == 0) {
        return(data)
    }

    # Parse selections safely
    parts <- strsplit(selections, "__", fixed = TRUE)
    valid_parts <- Filter(function(x) length(x) == 2, parts)
    if (length(valid_parts) == 0) {
        return(data)
    }

    # Build filter list
    filter_list <- lapply(
        split(valid_parts, vapply(valid_parts, `[[`, character(1), 1)),
        function(lst) vapply(lst, `[[`, character(1), 2)
    )

    keep_rows <- rep(TRUE, nrow(data))
    for (colname in names(filter_list)) {
        if (colname %in% names(data)) {
            keep_rows <- keep_rows & data[[colname]] %in% filter_list[[colname]]
        } else {
            warning(sprintf(
                "Column '%s' not found in data; skipping filter.",
                colname
            ))
        }
    }

    # Always return with the same columns as input
    out <- data[keep_rows, , drop = FALSE]
    if (nrow(out) == 0) {
        out <- data[0, , drop = FALSE]
    }

    # subset the full expression matrix by row without re-running filters.
    attr(out, "row_idx") <- which(keep_rows)
    return(out)
}


# Helper function to apply secondary filters
apply_secondary_filters <- function(data, inputs, keep_gene_matrix = TRUE, genes = NULL) {
    filtered <- data
    if ("n_feature_rna" %in% names(data) && "n_count_rna" %in% names(data)) {
        filtered <- filtered %>%
            filter(n_feature_rna > 0 & n_count_rna > 0)
    }

    # Select relevant columns. The list comes from config/data_columns.yml.
    metadata_cols <- app_metadata_columns()

    # For gene expression data, keep all columns (metadata + gene expression)
    # For regular metadata, select only metadata columns
    available_metadata_cols <- intersect(metadata_cols, names(filtered))
    non_metadata_cols_count <- length(setdiff(names(filtered), metadata_cols))

    if (!keep_gene_matrix) {
        if (!is.null(genes) && length(genes) > 0) {
            keep_cols <- setdiff(names(filtered), genes)
            keep_cols <- intersect(keep_cols, names(filtered))
            keep_cols <- unique(c(keep_cols, available_metadata_cols))
            filtered <- filtered %>% select(any_of(keep_cols))
        } else {
            filtered <- filtered %>% select(any_of(metadata_cols))
        }
    } else {
        if (non_metadata_cols_count > 10) {
            filtered <- filtered
        } else {
            filtered <- filtered %>% select(any_of(metadata_cols))
        }
    }


    # Dynamically apply filters for each non-empty input (on the filtered view)
    for (col in names(inputs)) {
        values <- inputs[[col]]
        if (!is.null(values) && length(values) > 0 && col %in% names(filtered)) {
            filtered <- filtered %>% filter(.data[[col]] %in% values)
        }
    }

    # Create row indices based on original data before any column selection/filtering
    original <- data
    if (!is.null(original) && nrow(original) > 0) {
        keep_rows <- rep(TRUE, nrow(original))

        # Apply the RNA QC filters if present
        if ("n_feature_rna" %in% names(original) && "n_count_rna" %in% names(original)) {
            keep_rows <- keep_rows & (original$n_feature_rna > 0 & original$n_count_rna > 0)
        }

        # Apply the dynamic inputs - THIS IS THE KEY FILTERING STEP
        if (!is.null(inputs) && length(inputs) > 0) {
            for (col in names(inputs)) {
                values <- inputs[[col]]
                if (!is.null(values) && length(values) > 0 && col %in% names(original)) {
                    keep_rows <- keep_rows & (original[[col]] %in% values)
                }
            }
        }

        # Attach row indices (positions in the original data)
        idx <- which(keep_rows)
        attr(filtered, "row_idx") <- idx
    } else {
        attr(filtered, "row_idx") <- integer(0)
    }

    return(filtered)
}
