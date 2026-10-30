# The embedding plots drawn so far, shared by every session.
# =============================================================================
# The same view (settings, embedding, filters, data type) is built once, by
# whichever session asks first; the cache holds a few views, the least
# recently used one is dropped. Startup draws the view the Embeddings tab
# opens with.
# =============================================================================

EMBEDDING_PLOT_CACHE <- shared_cache(size = 6)

# The plot of `params` (see embedding_figure()), from the cache or built by
# `build()`. The axes count only for 2D plots: a 3D plot shows the first
# three components whatever the axis menus say.
cached_embedding_plot <- function(params, plot_type, dual, filters, data_type, build) {
    settings <- params
    if (!identical(settings$plot, "2d")) {
        settings$xaxis <- NULL
        settings$yaxis <- NULL
    }
    key <- make_cache_key("embedding_plot", data_type, plot_type, dual, settings,
                          filters, attr(filters, "exclude"))
    cache_fetch(EMBEDDING_PLOT_CACHE, key, function() with_plot_payload(build()))
}

# Draws the plot shown when the Embeddings tab opens (EMBEDDING_PLOT_DEFAULTS,
# no filters) into the cache. Called once the app has started (global.R).
warm_embedding_plot_cache <- function() {
    defaults <- EMBEDDING_PLOT_DEFAULTS
    params <- defaults[c("color", "genes", "check", "combine", "xaxis", "yaxis", "size", "plot",
                         "legend_pos", "legend_size")]
    plot_type <- paste0(defaults$embedding, if (defaults$integrated) "_i" else "_u")
    filters <- empty_filter_inputs()
    loaded <- expression_data(defaults$data_type)
    cached_embedding_plot(
        params, plot_type, FALSE, filters, defaults$data_type,
        function() embedding_figure(filtered_cells(filters), params, plot_type, FALSE,
                                    gene_names = loaded$genes, expression = loaded$expression)
    )
    invisible(NULL)
}
