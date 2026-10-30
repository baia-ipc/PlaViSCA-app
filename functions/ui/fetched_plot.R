# An embedding plot that the browser downloads over HTTP with a progress bar
# (server/plot_payload.R, www/fetched_plot.js).

# The plot `id`: the plotly widget `id` of the given `height`, the progress
# bar over it, in the output `<id>_payload` (render_plot_payload()).
fetched_plot_output <- function(id, height = "100%") {
    div(
        id = paste0(id, "_payload"),
        class = "fetched-plot",
        style = paste0("height:", height, ";"),
        plotlyOutput(id, height = height),
        div(
            class = "plot-progress",
            role = "status",
            div(class = "plot-progress-text"),
            div(class = "plot-progress-bar", div(class = "plot-progress-fill"))
        )
    )
}
