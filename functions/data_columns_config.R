# Loads config/data_columns.yml and exposes lookups for the logical field ->
# actual data column mapping. See config/data_columns.yml for the field list.

.data_columns_config <- yaml::read_yaml("config/data_columns.yml")$fields

# Real data column name for a logical field key, e.g. "development_phase" -> "parasite_stage"
get_data_column <- function(key) {
  field <- .data_columns_config[[key]]
  if (is.null(field)) {
    stop(sprintf("Unknown data field '%s' - add it to config/data_columns.yml", key))
  }
  field$column
}

# Display label for a logical field key, e.g. "development_phase" -> "Development phase"
get_data_label <- function(key) {
  field <- .data_columns_config[[key]]
  if (is.null(field)) {
    stop(sprintf("Unknown data field '%s' - add it to config/data_columns.yml", key))
  }
  field$label
}

# Named choices list for selectizeInput/selectInput, e.g.
# field_choices(c("study_label", "development_phase")) ->
#   list("Study label" = "study_label", "Development phase" = "parasite_stage")
# Values are the resolved data columns; names stay the logical field keys.
field_choices <- function(keys) {
  choices <- lapply(keys, get_data_column)
  names(choices) <- vapply(keys, get_data_label, character(1))
  choices
}

# Display label for a resolved data column, e.g. "parasite_stage" -> "Development phase"
# (the inverse of get_data_column). Falls back to `column` itself if it isn't
# configured (e.g. "genes" or an ad-hoc gene expression column).
get_label_for_column <- function(column) {
  key <- Find(
    function(k) identical(.data_columns_config[[k]]$column, column),
    names(.data_columns_config)
  )
  if (is.null(key)) column else .data_columns_config[[key]]$label
}

# Checks that every configured column actually exists in `df`; warns (does not
# stop the app) listing any mismatch so a data refresh with renamed columns is
# caught immediately instead of silently degrading into wrong plots.
validate_data_columns <- function(df) {
  missing <- Filter(
    function(key) !(.data_columns_config[[key]]$column %in% colnames(df)),
    names(.data_columns_config)
  )
  if (length(missing) > 0) {
    warning(sprintf(
      "config/data_columns.yml: the following fields map to columns missing from the data: %s",
      paste(sprintf("%s -> %s", missing, vapply(missing, get_data_column, character(1))), collapse = ", ")
    ))
  }
  invisible(length(missing) == 0)
}
