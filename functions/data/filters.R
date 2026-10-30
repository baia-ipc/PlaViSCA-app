# Filtering cells by the shared filter widgets.
# =============================================================================
# Reads the filter selections of one tab and applies them to the metadata
# (mr_data). The expression matrix is not touched: the selected cells are
# given as row indices, which address its rows. Field names come from
# config/data_columns.yml.
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

# Input ID of the "exclude" switch of a filter.
filter_exclude_id <- function(key, suffix = "") paste0(key, "_exclude", suffix)

# Collects the current filter widget selections into a named list keyed by the
# actual data column names (via get_data_column()), suitable for
# apply_secondary_filters(). `suffix` selects which tab's set
# of widgets to read (e.g. "_meta", "_table", "_qc", "_gex", or "" for Embeddings).
# The attribute "exclude" names the columns whose filter has its "exclude"
# switch on: their selected values are left out instead of kept.
collect_filter_inputs <- function(input, suffix = "") {
    columns <- vapply(FILTER_FIELDS, get_data_column, character(1))
    out <- setNames(
        lapply(FILTER_FIELDS, function(field) input[[paste0(field, suffix)]]),
        columns
    )
    excluded <- vapply(FILTER_FIELDS, function(field) {
        isTRUE(input[[filter_exclude_id(field, suffix)]])
    }, logical(1))
    attr(out, "exclude") <- unname(columns[excluded])
    out
}

# The filter values of a tab with nothing selected (every filter keeps all
# cells), as collect_filter_inputs() gives them.
empty_filter_inputs <- function() {
    columns <- vapply(FILTER_FIELDS, get_data_column, character(1))
    out <- setNames(vector("list", length(columns)), columns)
    attr(out, "exclude") <- character(0)
    out
}

# TRUE for the values a filter keeps: those selected or, with the filter's
# "exclude" switch on, those not selected. An empty selection keeps all.
filter_keeps <- function(x, selected, exclude = FALSE) {
    if (length(selected) == 0) return(rep(TRUE, length(x)))
    if (exclude) !(x %in% selected) else x %in% selected
}

# The values a filter lets through, out of `all_values`: NULL (no
# constraint) for an empty selection, else the selection or, excluding, the
# other values.
filter_effective_values <- function(selected, exclude, all_values) {
    if (length(selected) == 0) return(NULL)
    if (isTRUE(exclude)) setdiff(all_values, selected) else selected
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

# The cells of `data` (the metadata) selected by the filter values `inputs`
# (see collect_filter_inputs()): the cells with at least one count and one
# gene, kept by every filter with a selection (an empty selection keeps all;
# a filter in attr "exclude" leaves its selection out instead). Returns the
# selected rows, with their positions in `data` in the attribute "row_idx":
# these are also their rows in the expression matrix.
apply_secondary_filters <- function(data, inputs) {
    keep <- rep(TRUE, nrow(data))
    if (all(c("n_feature_rna", "n_count_rna") %in% names(data))) {
        keep <- keep & data$n_feature_rna > 0 & data$n_count_rna > 0
    }
    excluded <- attr(inputs, "exclude")
    for (col in names(inputs)) {
        values <- inputs[[col]]
        if (length(values) > 0 && col %in% names(data)) {
            keep <- keep & filter_keeps(data[[col]], values, col %in% excluded)
        }
    }
    row_idx <- which(keep)
    out <- data[row_idx, , drop = FALSE]
    attr(out, "row_idx") <- row_idx
    out
}

# The rows of mr_data selected by a set of filter values (see
# apply_secondary_filters()). The row positions found for a set of filter
# values are kept for every session: the second request with the same
# filters, from any session, does not filter again.
FILTERED_CELLS_CACHE <- shared_cache(size = 50)

filtered_cells <- function(inputs) {
    key <- make_cache_key("cells", inputs, attr(inputs, "exclude"))
    row_idx <- cache_fetch(FILTERED_CELLS_CACHE, key, function() {
        attr(apply_secondary_filters(mr_data, inputs), "row_idx")
    })
    out <- mr_data[row_idx, , drop = FALSE]
    attr(out, "row_idx") <- row_idx
    out
}
