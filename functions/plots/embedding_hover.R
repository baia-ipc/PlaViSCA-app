# Hover of the cells of an embedding plot (colour value, Study, Detailed
# stage, X, Y and Z), and the traces the cells are drawn in.
#
# Nothing is sent per cell but its coordinates and colour: the cells are
# drawn in one trace per combination of colour category (if the colour is a
# category), study and detailed stage, and the hover is a plotly
# hovertemplate written once per trace, with the category, the study and the
# stage of its cells. A numeric colour (gene expression) is read from the
# marker colour of the cell, the coordinates from the plotted values. The
# traces of a category share a legend group with one legend entry, so that
# the legend lists each category once and toggles all its cells.
#
# The exception is a plot with its own value string per cell (the genes of
# "Each gene separately"): its cells are drawn in one trace, in their own
# order, with the value as the `customdata` of the point and the study and
# stage as its `hovertext` and `text`.
#
# A double-click or right-click on a cell sends the trace and the point
# (www/script.js); the rows of the plotted cells of each trace are kept with
# the plot (attribute "cell_rows", plotted_cell()) to find the cell.

# The values of the configured column `key` (e.g. "detailed_stage") of the
# cells `df`, as displayed: one per cell, missing values (and a column
# absent from `df`) as the missing label of the column.
hover_column <- function(df, key) {
  col <- get_data_column(key)
  values <- if (col %in% names(df)) df[[col]] else NA
  values <- rep_len(as.character(values), nrow(df))
  display_values(col, relabel_missing(values, missing_label_for(col)))
}

# The detailed stage of the cells `df`, as displayed.
hover_stages <- function(df) {
  hover_column(df, "detailed_stage")
}

# The traces of a plot of the cells `df` coloured by `color`: a list with,
# per trace, `rows` (the rows of `df` it draws), `category` (the value of
# `color` of its cells; NULL for a numeric colour), `study` and `stage` (the
# study and detailed stage of its cells) and `legend` (TRUE for the first
# trace of a category, which carries its legend entry). With `per_cell`, one
# trace of all the cells, whose study and stage are sent per cell (`study`
# and `stage` NULL).
embedding_trace_groups <- function(df, color, is_categorical, per_cell = FALSE) {
  rows <- seq_len(nrow(df))
  if (per_cell) {
    return(list(list(rows = rows, category = NULL, study = NULL, stage = NULL, legend = FALSE)))
  }
  category <- if (is_categorical) df[[color]] else rep("", nrow(df))
  if (!is.factor(category)) category <- factor(category)
  studies <- factor(hover_column(df, "study_label"))
  stages <- factor(hover_stages(df))
  groups <- list()
  for (level in levels(category)) {
    in_level <- rows[!is.na(category) & category == level]
    if (length(in_level) == 0) next
    first <- TRUE
    for (study in levels(droplevels(studies[in_level]))) {
      in_study <- in_level[studies[in_level] == study]
      for (stage in levels(droplevels(stages[in_study]))) {
        in_stage <- in_study[stages[in_study] == stage]
        groups[[length(groups) + 1]] <- list(
          rows = in_stage, category = if (is_categorical) level, study = study, stage = stage,
          legend = is_categorical && first
        )
        first <- FALSE
      }
    }
  }
  groups
}

# Hovertemplate of a trace of cells coloured by `color`, named
# `legend_title`: `category` is the value of a categorical trace (NULL: a
# numeric colour, read from the marker colour of each cell, or, with
# `per_cell_value`, a value string sent with each cell), `study` and `stage`
# the study and detailed stage of its cells (NULL: sent per cell), `n_dims`
# 2 or 3. A plot coloured by the study or the stage names it once.
hover_template <- function(legend_title, color, category, study, stage, n_dims = 2,
                           per_cell_value = FALSE) {
  value <- if (per_cell_value) "%{customdata}" else if (is.null(category)) "%{marker.color}" else category
  lines <- paste0(legend_title, ": ", value)
  if (!identical(color, get_data_column("study_label"))) {
    study <- if (is.null(study)) "%{hovertext}" else study
    lines <- c(lines, paste0(get_data_label("study_label"), ": ", study))
  }
  if (!identical(color, get_data_column("detailed_stage"))) {
    stage <- if (is.null(stage)) "%{text}" else stage
    lines <- c(lines, paste0(get_data_label("detailed_stage"), ": ", stage))
  }
  coords <- c("X: %{x:.2f}", "Y: %{y:.2f}", "Z: %{z:.2f}")[seq_len(n_dims)]
  paste0(paste(c(lines, coords), collapse = "<br>"), "<extra></extra>")
}

# The hover of the traces of the plot `p` of the cells `df`, coloured by
# `color` (named `legend_title`): `groups` are the traces, in the order they
# were added (embedding_trace_groups()); `categories` the category of each
# trace as written in the hover (e.g. in italics), NULL for a numeric
# colour; `values` a value string per cell to show instead (NULL: none).
# The values are set on the built traces, so that plotly does not repeat
# the template for every cell. The rows of the cells of each trace are kept
# as attribute "cell_rows" of the plot.
add_embedding_hover <- function(p, df, color, legend_title, groups, categories = NULL,
                                n_dims = 2, values = NULL) {
  per_cell <- !is.null(values)
  p <- plotly_build(p)
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    category <- if (is.null(categories)) NULL else categories[[i]]
    attrs <- list(
      hovertemplate = hover_template(legend_title, color, category, group$study, group$stage, n_dims,
                                     per_cell_value = per_cell)
    )
    if (per_cell) attrs$customdata <- I(as.character(values[group$rows]))
    if (is.null(group$stage)) attrs$text <- I(hover_stages(df)[group$rows])
    if (is.null(group$study)) attrs$hovertext <- I(hover_column(df, "study_label")[group$rows])
    if (!is.null(group$category)) attrs$legendgroup <- group$category
    p$x$data[[i]] <- utils::modifyList(p$x$data[[i]], attrs)
  }
  attr(p, "cell_rows") <- lapply(groups, function(group) group$rows)
  p
}

# The row, in the plotted cells, of point `point` of trace `curve` (both
# counted from 0, as plotly numbers them) of a plot whose traces draw the
# rows `cell_rows` (attribute "cell_rows" of the plot); NA when there is no
# such point.
plotted_cell <- function(cell_rows, curve, point) {
  curve <- suppressWarnings(as.integer(curve))
  point <- suppressWarnings(as.integer(point))
  if (is.null(cell_rows) || length(curve) != 1 || length(point) != 1 || is.na(curve) || is.na(point) ||
      curve < 0 || curve >= length(cell_rows)) {
    return(NA_integer_)
  }
  rows <- cell_rows[[curve + 1]]
  if (point < 0 || point >= length(rows)) NA_integer_ else as.integer(rows[point + 1])
}

# Colour values of the cells at 3 significant digits; whole numbers (counts)
# are kept as they are.
compact_colour_values <- function(x) {
  x <- as.numeric(x)
  whole <- is.na(x) | x == round(x)
  if (all(whole)) x else signif(x, 3)
}

# Plot coordinates rounded to 3 decimals.
compact_coordinates <- function(x) {
  round(as.numeric(x), 3)
}

# JSON of a plot as sent to the browser, with numbers written with the digits
# they have (plotly's own serializer pads them to 17 significant digits, so
# that rounded coordinates would be sent as long as unrounded ones).
compact_plot_json <- function(x, ...) {
  jsonlite::toJSON(
    x,
    digits = NA, auto_unbox = TRUE, force = TRUE, null = "null", na = "null",
    time_format = "%Y-%m-%d %H:%M:%OS6", ...
  )
}

# The plot `p`, sent to the browser with compact_plot_json().
with_compact_json <- function(p) {
  attr(p$x, "TOJSON_FUNC") <- compact_plot_json
  p
}
