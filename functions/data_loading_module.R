# Data Loading and Caching Module
# Functions for loading, caching, and managing data in PlaViSCA

#' Initialize data caching reactive values
#' @return reactiveValues object for caching
initialize_data_cache <- function() {
    reactiveValues(
        raw = NULL,
        normalize = NULL,
        current_type = NULL,
        current_filter = NULL
    )
}

#' Load dataset with caching
#' @param data_type Type of data to load ("raw" or "normalize")
#' @param cached_data Reactive values object containing cached data
#' @return Loaded dataset
load_dataset <- function(data_type, cached_data) {
    if (!is.null(cached_data[[data_type]])) {
        return(cached_data[[data_type]])
    }

    dataset <- switch(data_type,
        raw = {
            if (exists("DATA_PATHS") && !is.null(DATA_PATHS$raw_df)) {
                load_data(DATA_PATHS$raw_df)
            } else {
                NULL
            }
        },
        normalize = {
            if (exists("norm_df")) norm_df else NULL
        }
    )

    if (!is.null(dataset)) {
        cached_data[[data_type]] <- dataset
        # Only gc after loading large datasets
        if (data_type == "raw") {
            gc(verbose = FALSE)
        }
    }

    return(dataset)
}

#' Get current data type from inputs
#' @param input Shiny input object
#' @return Current data type string
get_current_data_type <- function(input) {
    if (input$tabset == "Gene expression") {
        input$data_type_exp
    } else if (input$tabset == "Embeddings") {
        input$data_type
    } else {
        "normalize" # default when launching
    }
}

#' Setup data type synchronization observers
#' @param input Shiny input object
#' @param session Shiny session object
#' @param global_loading Reactive value for global loading state
setup_data_sync_observers <- function(input, session, global_loading) {
    # Synchronize data type selections between tabs using helper functions
    create_data_type_sync_observer(
        input = input,
        session = session,
        input_name = "data_type", # embeddings tabset
        target_input = "data_type_exp",
        expected_tabset = "Embeddings",
        global_loading = global_loading
    )

    create_data_type_sync_observer(
        input = input,
        session = session,
        input_name = "data_type_exp", # from exp tabset
        target_input = "data_type",
        expected_tabset = "Gene expression",
        global_loading = global_loading
    )
}

#' Create main gene data reactive
#' @param data_loading_trigger Reactive trigger for data loading
#' @param app_initialized Reactive value indicating app initialization
#' @param cached_data Reactive values for cached data
#' @param sessionData Session environment for caching
#' @param input Shiny input object
#' @param gff_data GFF annotation data
#' @return Event reactive for gene data
create_gene_data_reactive <- function(data_loading_trigger, app_initialized, 
                                     cached_data, sessionData, input, gff_data) {
    eventReactive(
        data_loading_trigger(),
        {
            if (!app_initialized()) {
                return(NULL)
            }

            current_type <- get_current_data_type(input)
            req(current_type)

            # Check if we can reuse cached data based ONLY on data type
            if (
                !is.null(cached_data$current_type) &&
                    cached_data$current_type == current_type
            ) {
                return(sessionData$last_gene_data)
            }

            # Show progress bar when loading new data
            withProgress(message = "Loading data", value = 30, {
                display_name <- switch(current_type,
                    raw = "Raw",
                    normalize = "Normalized"
                )

                # Load dataset (from cache if available)
                dataset <- load_dataset(current_type, cached_data)

                result <- list(
                    data = dataset,
                    genes = genes(dataset, gff_data)
                )

                # Cache the current state (only data type, no filter)
                cached_data$current_type <- current_type
                sessionData$last_gene_data <- result
                return(result)
            })
        },
        ignoreNULL = FALSE
    )
}

#' Create filtered gene data reactive
#' @param gene_data Reactive expression for gene data
#' @param input Shiny input object
#' @param default_df_filter Reactive value for default filter
#' @param sessionData Session environment for caching
#' @param cached_data Reactive values for cached data
#' @return Reactive expression for filtered gene data
create_filtered_gene_data_reactive <- function(gene_data, input, 
                                               default_df_filter, sessionData, 
                                               cached_data) {
    reactive({
        req(gene_data())

        selections <- input$apply_filter

        # If no selections made (empty or null), use default filter
        if (is.null(selections) || length(selections) == 0) {
            selections <- default_df_filter()
        }

        req(selections)

        # Create cache key for filtered gene data
        cache_key <- make_cache_key(
            "filtered_gene_data",
            get_current_data_type(input),
            selections
        )

        # Check cache first to avoid reprocessing
        if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
            return(sessionData[[cache_key]])
        }

        # Parse filter selections
        parsed_inputs <- list()
        if (!is.null(selections) && length(selections) > 0) {
            parts <- strsplit(selections, "__", fixed = TRUE)
            valid_parts <- Filter(function(x) length(x) == 2, parts)
            if (length(valid_parts) > 0) {
                filter_list <- lapply(
                    split(valid_parts, vapply(valid_parts, `[[`, character(1), 1)),
                    function(lst) vapply(lst, `[[`, character(1), 2)
                )
                parsed_inputs <- filter_list
            }
        }

        filtered_meta <- tryCatch(
            {
                apply_secondary_filters(
                    gene_data()$data, 
                    parsed_inputs, 
                    keep_gene_matrix = FALSE, 
                    genes = if (!is.null(gene_data()$genes)) gene_data()$genes$gene else NULL
                )
            },
            error = function(e) {
                # Fallback to safe fun_filter_df behavior if something unexpected
                fun_filter_df(gene_data()$data, selections)
            }
        )

        # Get row indices if available from the filtered metadata
        row_idx <- attr(filtered_meta, "row_idx")
        
        # Get the full dataset (with gene expression matrix)
        full_df <- if (!is.null(cached_data[[get_current_data_type(input)]])) {
            cached_data[[get_current_data_type(input)]]
        } else {
            gene_data()$data
        }
        
        result <- list(
            data = filtered_meta,
            full = full_df,
            row_idx = row_idx,
            genes = gene_data()$genes
        )

        # Cache the result
        sessionData[[cache_key]] <- result

        return(result)
    })
}
