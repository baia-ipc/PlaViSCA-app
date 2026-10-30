# Output of an embedding plot downloaded by the browser over HTTP.
#
# The output value sent over the websocket is small: the address and size of
# the JSON of the plot, which the session serves at a URL of its own. The
# browser downloads it with a progress bar and draws it in the plotly widget
# (www/fetched_plot.js, fetched_plot_output() in functions/ui/fetched_plot.R).
# HTTP responses are compressed by the web server in front of the app;
# websocket messages are not.

# Render function of the output `<id>_payload` of the plot `id` (a
# fetched_plot_output()); `expr` gives the plot (a plotly object, or NULL
# for nothing to draw).
render_plot_payload <- function(session, id, expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  shiny::createRenderFunction(func, function(p, session, name, ...) {
    plot_payload_value(session, id, p)
  })
}

# The output value of the plot `p` of the output `id`: `plot` (the id),
# `bytes` (the size of the JSON) and `url` (where the session serves it).
# A session that cannot serve it (a test session) gets the JSON inline, as
# `json`. NULL for no plot.
plot_payload_value <- function(session, id, p) {
  if (is.null(p)) {
    return(NULL)
  }
  json <- payload_of(p)
  value <- list(plot = id, bytes = nchar(json, type = "bytes"))
  url <- session$registerDataObj(id, json, serve_plot_json)
  if (is.null(url) || !nzchar(url)) {
    value$json <- json
  } else {
    value$url <- url
  }
  value
}

# Serves the JSON `data` of a plot.
serve_plot_json <- function(data, req) {
  shiny::httpResponse(200, "application/json; charset=utf-8", data)
}
