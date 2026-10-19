# Keeping tabs in sync, and cache keys for per-session data caches.

# Cache key made of the given values (vectors are sorted and joined).
make_cache_key <- function(...) {
    args <- list(...)
    parts <- lapply(args, function(x) {
        if (is.null(x)) {
            return("NULL")
        } else if (length(x) > 1) {
            return(paste(sort(as.character(x)), collapse = "|"))
        } else {
            return(as.character(x))
        }
    })
    paste(parts, collapse = "__")
}

# The Embeddings and Gene expression tabs each have a "Data type" control
# (data_type, data_type_exp): a change on the tab being viewed is copied to
# the other tab. A change on the Gene expression tab sets `global_loading`
# for 1.5 s, while the data are reloaded.
sync_data_type_controls <- function(input, session, global_loading) {
    observeEvent(input$data_type, {
        if (identical(input$tabset, "Embeddings")) {
            updateRadioButtons(session, "data_type_exp", selected = input$data_type)
        }
    }, ignoreInit = TRUE)

    observeEvent(input$data_type_exp, {
        if (identical(input$tabset, "Gene expression")) {
            global_loading(TRUE)
            updateRadioButtons(session, "data_type", selected = input$data_type_exp)
            later(function() global_loading(FALSE), 1.5)
        }
    }, ignoreInit = TRUE)
}
