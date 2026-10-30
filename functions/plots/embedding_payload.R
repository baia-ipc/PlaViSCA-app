# The JSON of an embedding plot as the browser draws it.
#
# An embedding plot is large (a few MB for the whole atlas). It is not sent
# over Shiny's websocket, which is not compressed and gives no progress, but
# downloaded by the browser over HTTP, with a progress bar
# (server/plot_payload.R, www/fetched_plot.js). The JSON is made once per
# plot and kept with it in the plot cache (embedding_cache.R).

# The JSON of the plot `p`, as a string: what renderPlotly() would send.
plot_payload <- function(p) {
    as.character(plotly::renderPlotly(p)(NULL, "plot"))
}

# The plot `p` with its JSON in the attribute "payload" (NULL stays NULL).
with_plot_payload <- function(p) {
    if (is.null(p)) {
        return(NULL)
    }
    attr(p, "payload") <- plot_payload(p)
    p
}

# The JSON of the plot `p`: the one kept with it, or made now.
payload_of <- function(p) {
    json <- attr(p, "payload")
    if (is.null(json)) plot_payload(p) else json
}
