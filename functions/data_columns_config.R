# Configuration accessors for config/data_columns.yml.
# =============================================================================
# This file is the ONLY place the app is allowed to turn a logical field key
# into a physical metadata column name, a display label, a biological ordering
# or a colour. Everything else in the app must go through these helpers so a
# schema change is a one-file change.
#
# See config/data_columns.yml for the field list and the rationale.
# =============================================================================

.data_columns_config_path <- function() {
    Sys.getenv("PLAVISCA_DATA_COLUMNS_CONFIG", unset = "config/data_columns.yml")
}

.data_columns_raw <- yaml::read_yaml(.data_columns_config_path())
.data_columns_config <- .data_columns_raw$fields

`%||%` <- function(a, b) if (is.null(a)) b else a

# Display string used wherever a true R NA is shown to a user.
MISSING_VALUE_LABEL <- .data_columns_raw$missing_label %||% "Missing"

# Strings that must never reach the UI as if they were real categories. The
# startup checks make sure app-visible fields contain none of them; they are
# also relabelled on display as a safeguard.
LITERAL_MISSING_TOKENS <- c("NA", "N/A", "n/a", "NaN", "None", "none", "null", "NULL", "")

.config_field <- function(key) {
    field <- .data_columns_config[[key]]
    if (is.null(field)) {
        stop(sprintf(
            "Unknown data field '%s' - add it to %s",
            key, .data_columns_config_path()
        ), call. = FALSE)
    }
    field
}

# --------------------------------------------------------------- basic lookups

#' Physical data column for a logical field key,
#' e.g. "detailed_stage" -> "stage".
get_data_column <- function(key) {
    .config_field(key)$column
}

#' Display label for a logical field key,
#' e.g. "broad_stage" -> "Broad life-cycle stage".
get_data_label <- function(key) {
    .config_field(key)$label
}

#' Long-form help/tooltip text for a logical field key, or NULL if none.
get_data_help <- function(key) {
    help <- .config_field(key)$help
    if (is.null(help)) NULL else trimws(gsub("\\s+", " ", help))
}

#' All configured logical keys, optionally restricted to one `role`.
data_field_keys <- function(role = NULL) {
    keys <- names(.data_columns_config)
    if (is.null(role)) {
        return(keys)
    }
    Filter(function(k) identical(.data_columns_config[[k]]$role, role), keys)
}

#' Logical keys that must exist in the loaded data for the app to run.
required_field_keys <- function() {
    Filter(function(k) isTRUE(.data_columns_config[[k]]$required), names(.data_columns_config))
}

#' Physical columns the app requires to be present.
required_data_columns <- function() {
    unique(vapply(required_field_keys(), get_data_column, character(1)))
}

#' Named choices list for selectInput()/selectizeInput(): names are display
#' labels, values are the resolved physical column names.
field_choices <- function(keys) {
    choices <- lapply(keys, get_data_column)
    names(choices) <- vapply(keys, get_data_label, character(1))
    choices
}

#' Inverse of get_data_column(): physical column -> display label. Falls back
#' to the column name itself for unconfigured columns (e.g. a gene id).
get_label_for_column <- function(column) {
    key <- key_for_column(column)
    if (is.null(key)) column else get_data_label(key)
}

#' Inverse of get_data_column(): physical column -> logical key, or NULL.
key_for_column <- function(column) {
    Find(
        function(k) identical(.data_columns_config[[k]]$column, column),
        names(.data_columns_config)
    )
}

# ------------------------------------------------------------- field groupings

filter_field_keys <- function() .data_columns_raw$filter_fields
category_field_keys <- function() .data_columns_raw$category_fields
detail_table_field_keys <- function() .data_columns_raw$detail_table_fields

#' Physical columns behind the shared filter row.
filter_data_columns <- function() {
    vapply(filter_field_keys(), get_data_column, character(1))
}

#' "Color by" / "Group by" choices, plus the pseudo-option for genes.
category_choices <- function(include_genes = TRUE) {
    choices <- field_choices(category_field_keys())
    if (include_genes) choices <- c(choices, list("Genes" = "genes"))
    choices
}

#' TRUE when a field's values should be ordered numerically rather than by the
#' configured vocabulary / alphabetically.
field_is_numeric_sorted <- function(key) {
    isTRUE(.config_field(key)$numeric_sort)
}

app_expected_studies <- function() as.character(.data_columns_raw$expected_studies)
app_unsupported_columns <- function() as.character(.data_columns_raw$unsupported_columns)
app_note <- function(name) {
    note <- .data_columns_raw$notes[[name]]
    if (is.null(note)) NULL else trimws(gsub("\\s+", " ", note))
}

# --------------------------------------------------------------- vocabularies

#' Configured biological ordering for a logical field key (character(0) when
#' the field has no documented ordering).
get_field_vocabulary <- function(key) {
    vocab <- .data_columns_raw$vocabularies[[key]]
    if (is.null(vocab)) character(0) else as.character(vocab)
}

#' Order a vector of observed values by the documented biological ordering.
#' Values present in the data but absent from the vocabulary are appended
#' (sorted) rather than dropped, so an unexpected level is visible instead of
#' silently disappearing. Levels in the vocabulary but absent from the data are
#' never invented.
order_field_values <- function(key, values) {
    values <- unique(values[!is.na(values)])
    if (length(values) == 0) {
        return(character(0))
    }
    if (field_is_numeric_sorted(key)) {
        num <- suppressWarnings(as.numeric(as.character(values)))
        return(values[order(is.na(num), num, as.character(values))])
    }
    vocab <- get_field_vocabulary(key)
    values <- as.character(values)
    known <- vocab[vocab %in% values]
    unknown <- sort(setdiff(values, vocab))
    c(known, unknown)
}

#' Same as order_field_values() but keyed by physical column name; returns the
#' input unchanged when the column is not configured.
order_column_values <- function(column, values) {
    key <- key_for_column(column)
    if (is.null(key)) {
        values <- unique(values[!is.na(values)])
        num <- suppressWarnings(as.numeric(as.character(values)))
        if (length(values) > 0 && all(!is.na(num))) {
            return(values[order(num)])
        }
        return(sort(as.character(values)))
    }
    order_field_values(key, values)
}

#' Explicit colour overrides configured for a field, as a named character
#' vector (may be empty). Keyed by logical field key.
get_field_colors <- function(key) {
    cols <- .data_columns_raw$colors[[key]]
    if (is.null(cols)) {
        return(setNames(character(0), character(0)))
    }
    unlist(cols)
}

#' Colour overrides for a physical column name.
get_column_colors <- function(column) {
    key <- key_for_column(column)
    if (is.null(key)) {
        return(setNames(character(0), character(0)))
    }
    get_field_colors(key)
}

#' Human-readable display value for machine-ish category values,
#' e.g. treatment "study_control" -> "Study control". Underlying data is never
#' modified - this is display-only.
display_values <- function(column, values) {
    key <- key_for_column(column)
    if (is.null(key)) {
        return(as.character(values))
    }
    map <- .data_columns_raw$value_labels[[key]]
    out <- as.character(values)
    if (is.null(map)) {
        return(out)
    }
    for (raw in names(map)) {
        out[!is.na(out) & out == raw] <- map[[raw]]
    }
    out
}

italicised_columns <- function() {
    keys <- as.character(.data_columns_raw$italicised_fields %||% character(0))
    vapply(keys, get_data_column, character(1), USE.NAMES = FALSE)
}

# ------------------------------------------------------- missing-value display

#' Generic missing-value display helper. Converts true NA (and, defensively,
#' any residual literal "NA"/"None"/"" string) into the configured missing
#' label. Intended for display/labelling paths only - never call this on data
#' that is about to be used analytically.
relabel_missing <- function(x, label = MISSING_VALUE_LABEL) {
    out <- as.character(x)
    out[is.na(out) | out %in% LITERAL_MISSING_TOKENS] <- label
    out
}

#' TRUE for values that are missing either as true NA or as a literal
#' missing-ish string.
is_missing_value <- function(x) {
    chr <- as.character(x)
    is.na(chr) | chr %in% LITERAL_MISSING_TOKENS
}

# ------------------------------------------ broad / detailed stage compatibility

#' The documented detailed -> broad life-cycle stage mapping, as a named
#' character vector (names = detailed stage, values = broad stage).
stage_compatibility_map <- function() {
    unlist(.data_columns_raw$stage_compatibility)
}

#' Detailed stages compatible with the given broad stage selection.
#' An empty/NULL selection means "no constraint" and returns all detailed
#' stages in the configured biological order.
compatible_detailed_stages <- function(broad_selection = NULL, available = NULL) {
    map <- stage_compatibility_map()
    detailed <- names(map)
    if (!is.null(broad_selection) && length(broad_selection) > 0) {
        broad_selection <- broad_selection[!is_missing_value(broad_selection)]
        if (length(broad_selection) > 0) {
            detailed <- names(map)[map %in% broad_selection]
        }
    }
    if (!is.null(available)) {
        detailed <- intersect(detailed, unique(as.character(available)))
    }
    order_field_values("detailed_stage", detailed)
}

#' Broad stages compatible with the given detailed stage selection.
compatible_broad_stages <- function(detailed_selection = NULL, available = NULL) {
    map <- stage_compatibility_map()
    broad <- unname(map)
    if (!is.null(detailed_selection) && length(detailed_selection) > 0) {
        detailed_selection <- detailed_selection[!is_missing_value(detailed_selection)]
        if (length(detailed_selection) > 0) {
            broad <- unname(map[names(map) %in% detailed_selection])
        }
    }
    broad <- unique(broad)
    if (!is.null(available)) {
        broad <- intersect(broad, unique(as.character(available)))
    }
    order_field_values("broad_stage", broad)
}

#' TRUE when the (broad, detailed) filter selection can possibly match a cell.
#' Empty selections are always considered valid (they mean "no constraint").
stage_selection_is_possible <- function(broad_selection, detailed_selection) {
    broad_selection <- broad_selection[!is_missing_value(broad_selection)]
    detailed_selection <- detailed_selection[!is_missing_value(detailed_selection)]
    if (length(broad_selection) == 0 || length(detailed_selection) == 0) {
        return(TRUE)
    }
    map <- stage_compatibility_map()
    implied <- unname(map[names(map) %in% detailed_selection])
    length(intersect(implied, broad_selection)) > 0
}

#' Human-readable explanation of an impossible broad/detailed combination,
#' or NULL when the combination is possible.
stage_selection_message <- function(broad_selection, detailed_selection) {
    if (stage_selection_is_possible(broad_selection, detailed_selection)) {
        return(NULL)
    }
    map <- stage_compatibility_map()
    implied <- unique(unname(map[names(map) %in% detailed_selection]))
    sprintf(
        paste0(
            "No cells can match this combination: the selected life-cycle ",
            "stage(s) (%s) only occur in the %s, but the broad stage filter ",
            "is restricted to %s. Broad and detailed life-cycle stage are not ",
            "independent - clear one of the two filters."
        ),
        paste(detailed_selection, collapse = ", "),
        paste(implied, collapse = ", "),
        paste(broad_selection, collapse = ", ")
    )
}

# ------------------------------------------------------------------ validation

#' Checks that every *required* configured column exists in `df` and that no
#' configured field points at an unsupported column.
#'
#' Stops (rather than warns) by default: a column mismatch must never turn
#' silently into empty dropdowns or wrong plots.
validate_data_columns <- function(df, stop_on_error = TRUE) {
    problems <- character(0)

    missing <- Filter(
        function(key) !(get_data_column(key) %in% colnames(df)),
        required_field_keys()
    )
    if (length(missing) > 0) {
        problems <- c(problems, sprintf(
            "required field(s) map to columns missing from the data: %s",
            paste(sprintf("%s -> %s", missing, vapply(missing, get_data_column, character(1))),
                collapse = ", "
            )
        ))
    }

    unsupported <- app_unsupported_columns()
    bad <- Filter(function(key) get_data_column(key) %in% unsupported, names(.data_columns_config))
    if (length(bad) > 0) {
        problems <- c(problems, sprintf(
            "field(s) map to unsupported columns: %s",
            paste(sprintf("%s -> %s", bad, vapply(bad, get_data_column, character(1))),
                collapse = ", "
            )
        ))
    }

    if (length(problems) > 0 && stop_on_error) {
        stop(sprintf(
            "PlaViSca: the data do not match %s:\n  - %s",
            .data_columns_config_path(), paste(problems, collapse = "\n  - ")
        ), call. = FALSE)
    }
    invisible(problems)
}
