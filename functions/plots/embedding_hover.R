# Hover of the cells of an embedding plot (Cell, colour value, Study,
# Detailed stage, X, Y and Z), and the traces the cells are drawn in.
#
# The hover is a plotly hovertemplate, written once per trace, so that a cell
# carries only the values of its own: its barcode (the `customdata` of the
# point, also what a double-click or right-click sends to open the cell
# details, www/script.js), and its study as the `hovertext` of the point
# unless the plot is coloured by study. A numeric colour (gene expression)
# is read from the marker colour of the cell, the coordinates from the
# plotted values. A plot with its own value string per cell (the genes of
# "Each gene separately") sends it as a second customdata column.
#
# The cells of a categorical plot are drawn in one trace per category; the
# detailed stage is written in the template when the plot is coloured by it,
# and sent per cell, as the `text` of the point, otherwise (as for a numeric
# colour, whose cells are drawn in one trace).

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
# `color` of its cells; NULL for a numeric colour), `stage` (the detailed
# stage of its cells; NULL when it is sent per cell) and `legend` (TRUE for
# a trace with a legend entry).
embedding_trace_groups <- function(df, color, is_categorical) {
  rows <- seq_len(nrow(df))
  if (!is_categorical) {
    return(list(list(rows = rows, category = NULL, stage = NULL, legend = FALSE)))
  }
  category <- df[[color]]
  if (!is.factor(category)) category <- factor(category)
  levels <- levels(category)[tabulate(category, nlevels(category)) > 0]
  by_stage <- identical(color, get_data_column("detailed_stage"))
  stages <- if (by_stage) hover_stages(df)
  lapply(levels, function(level) {
    in_level <- rows[!is.na(category) & category == level]
    list(rows = in_level, category = level,
         stage = if (by_stage) stages[in_level][1], legend = TRUE)
  })
}

# Hovertemplate of a trace of cells coloured by `color`, named
# `legend_title`: `category` is the value of a categorical trace (NULL: a
# numeric colour, read from the marker colour of each cell, or, with
# `per_cell_value`, a value string sent with each cell), `stage` the
# detailed stage of its cells (NULL: sent per cell), `n_dims` 2 or 3.
hover_template <- function(legend_title, color, category, stage, n_dims = 2, per_cell_value = FALSE) {
  cell <- if (per_cell_value) "%{customdata[0]}" else "%{customdata}"
  value <- if (per_cell_value) "%{customdata[1]}" else if (is.null(category)) "%{marker.color}" else category
  study <- if (identical(color, get_data_column("study_label"))) category else "%{hovertext}"
  stage <- if (is.null(stage)) "%{text}" else stage
  coords <- c("<br>X: %{x:.2f}", "<br>Y: %{y:.2f}", "<br>Z: %{z:.2f}")[seq_len(n_dims)]
  paste0(
    "Cell: ", cell,
    "<br>", legend_title, ": ", value,
    "<br>", get_data_label("study_label"), ": ", study,
    "<br>", get_data_label("detailed_stage"), ": ", stage,
    paste(coords, collapse = ""),
    "<extra></extra>"
  )
}

# The hover of the traces of the plot `p` of the cells `df`, coloured by
# `color` (named `legend_title`): `groups` are the traces, in the order they
# were added (embedding_trace_groups()); `categories` the category of each
# trace as written in the hover (e.g. in italics), NULL for a numeric
# colour; `values` a value string per cell to show instead (NULL: none).
# The values are set on the built traces, so that plotly does not repeat
# the template for every cell.
add_embedding_hover <- function(p, df, color, legend_title, groups, categories = NULL,
                                n_dims = 2, values = NULL) {
  barcodes <- as.character(df[[get_data_column("barcode")]])
  studies <- if (identical(color, get_data_column("study_label"))) NULL else {
    hover_column(df, "study_label")
  }
  stages <- hover_stages(df)
  p <- plotly_build(p)
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    category <- if (is.null(categories)) NULL else categories[[i]]
    attrs <- list(
      hovertemplate = hover_template(legend_title, color, category, group$stage, n_dims,
                                     per_cell_value = !is.null(values)),
      customdata = if (is.null(values)) I(barcodes[group$rows]) else
        cbind(barcodes[group$rows], as.character(values[group$rows]))
    )
    if (is.null(group$stage)) attrs$text <- I(stages[group$rows])
    if (!is.null(studies)) attrs$hovertext <- I(studies[group$rows])
    p$x$data[[i]] <- utils::modifyList(p$x$data[[i]], attrs)
  }
  p
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
