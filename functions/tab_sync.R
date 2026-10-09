# Keeping tabs in sync, and cache keys for per-session data caches.

# Cache key generator
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

# Create a cache key for filtered gene expression inputs
filtered_gene_exp_cache_key <- function(input) {
    get_field <- function(name) {
        if (is.null(input)) {
            return(NULL)
        }
        if (is.list(input) && !is.null(input[[name]])) {
            return(input[[name]])
        }
        tryCatch(
            input[[name]],
            error = function(e) NULL
        )
    }

    do.call(
        make_cache_key,
        c(
            list("filtered_gene_exp"),
            lapply(paste0(FILTER_FIELDS, "_gex"), get_field),
            list(get_field("data_type_exp"), get_field("embedding"))
        )
    )
}

# Helper function to sync data types between tabs
sync_data_type <- function(
  session,
  from_input,
  to_input,
  new_value,
  global_loading,
  delay = 1.5
) {
    global_loading(TRUE)
    updateRadioButtons(
        session,
        to_input,
        selected = new_value
    )
    later(
        function() {
            tryCatch(
                {
                    global_loading(FALSE)
                },
                error = function(e) {
                    # Silent error handling
                }
            )
        },
        delay
    )
}

# Helper function to create data type
create_data_type_sync_observer <- function(
  input,
  session,
  input_name,
  target_input,
  expected_tabset,
  global_loading
) {
    observeEvent(
        input[[input_name]],
        {
            if (
                !is.null(input[[input_name]]) &&
                    !is.null(input$tabset) &&
                    input$tabset == expected_tabset
            ) {
                sync_data_type(
                    session = session,
                    from_input = input_name,
                    to_input = target_input,
                    new_value = input[[input_name]],
                    global_loading = global_loading
                )
            }
        },
        ignoreInit = TRUE
    )
}
