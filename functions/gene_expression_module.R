# Gene Expression Module
# Functions for handling gene expression analysis and visualization

#' Create filtered gene expression data reactive
#' @param gene_data Reactive expression for gene data
#' @param input Shiny input object
#' @param sessionData Session environment for caching
#' @param cached_data Reactive values for cached data
#' @param get_current_data_type_fn Function to get current data type
#' @return Debounced reactive expression for filtered gene expression data
create_filtered_gene_exp_reactive <- function(gene_data, input, sessionData, 
                                              cached_data, get_current_data_type_fn) {
    reactive({
        req(gene_data())

        current_inputs <- list(
            study_label = input$study_label,
            host_species = input$host_species,
            parasite_stage = input$parasite_stage,
            treatment = input$treatment,
            development_phase = input$development_phase,
            sample_type = input$sample_type,
            strain = input$strain,
            hour_post_invasion = input$hour_post_invasion,
            sequencer = input$sequencer,
            sc_technology = input$sc_technology
        )

        # Create cache key for filtered gene expression data
        cache_key <- filtered_gene_exp_cache_key(input)
        if (exists(cache_key, envir = sessionData, inherits = FALSE)) {
            return(sessionData[[cache_key]])
        }

        tryCatch(
            {
                # Get the full expression matrix from cached data
                full_df <- if (!is.null(cached_data[[get_current_data_type_fn(input)]])) {
                    cached_data[[get_current_data_type_fn(input)]]
                } else {
                    gene_data()$data
                }

                filt_meta <- apply_secondary_filters(
                    gene_data()$data,
                    current_inputs,
                    keep_gene_matrix = FALSE,
                    genes = if (!is.null(gene_data()$genes)) gene_data()$genes$gene else NULL
                )

                # Ensure row indices are available on the metadata result
                row_idx <- attr(filt_meta, "row_idx")

                result <- list(
                    data = filt_meta,
                    genes = gene_data()$genes,
                    full = full_df,
                    row_idx = row_idx
                )

                sessionData[[cache_key]] <- result

                return(result)
            },
            error = function(e) {
                return(NULL)
            }
        )
    }) %>% debounce(500)
}

#' Setup gene expression UI observers
#' @param input Shiny input object
#' @param session Shiny session object
#' @param gene_data Reactive expression for gene data
setup_gene_exp_ui_observers <- function(input, session, gene_data) {
    # Initialize dropdown
    observeEvent(
        gene_data(),
        {
            req(gene_data()$genes)

            initial_choices <- get_gene_choices(gene_data()$genes, FALSE)

            tryCatch(
                {
                    updateSelectizeInput(
                        session,
                        "genes_exp_0",
                        label = "Gene name",
                        choices = initial_choices,
                        options = list(
                            placeholder = "Select one or more genes",
                            maxItems = 10
                        ),
                        server = TRUE
                    )
                },
                error = function(e) {
                    # Silent error handling
                }
            )
        },
        once = TRUE
    )

    # Update dropdown when checkbox changes
    observeEvent(input$check_exp_0, {
        req(gene_data()$genes)

        choices <- get_gene_choices(gene_data()$genes, input$check_exp_0)
        
        current_selection <- input$genes_exp_0
        if (!is.null(current_selection) && all(current_selection %in% choices)) {
            default_selection <- current_selection
        } else {
            default_selection <- if (length(choices) > 0) choices[1] else NULL
        }

        tryCatch(
            {
                updateSelectizeInput(
                    session,
                    "genes_exp_0",
                    choices = choices,
                    selected = default_selection,
                    options = list(
                        placeholder = "Select one or more genes",
                        maxItems = 10
                    )
                )
            },
            error = function(e) {
                # Silent error handling
            }
        )
    })
}

#' Render gene expression plot
#' @param output Shiny output object
#' @param input Shiny input object
#' @param filtered_gene_exp_data Reactive expression for filtered gene expression data
#' @param global_loading Reactive value for global loading state
render_gene_exp_plot <- function(output, input, filtered_gene_exp_data, global_loading) {
    # Debounced gene selection to prevent flickering
    genes_exp_0_debounced <- reactive({
        input$genes_exp_0
    }) %>% debounce(800)
    
    group_exp_debounced <- reactive({
        input$group_exp
    }) %>% debounce(500)

    output$gene_exp_plotly <- renderPlotly({
        # Use debounced inputs
        selected_genes <- genes_exp_0_debounced()
        selected_group <- group_exp_debounced()
        
        # Validate inputs
        if (is.null(selected_genes) || length(selected_genes) == 0 || is.null(selected_group)) {
            return(create_empty_plot("Please select at least one gene and a grouping option"))
        }

        if (global_loading()) {
            return(create_empty_plot("Loading..."))
        }

        tryCatch({
            filtered_gene_exp <- filtered_gene_exp_data()
            if (is.null(filtered_gene_exp) || is.null(filtered_gene_exp$data)) {
                return(create_empty_plot("Data not loaded"))
            }
            
            filtered_data <- filtered_gene_exp$data
            raw_data <- list(
                data = filtered_gene_exp$full,
                genes = filtered_gene_exp$genes
            )
            
            full_matrix <- filtered_gene_exp$full
            row_idx <- attr(filtered_data, "row_idx")
            
            # Process gene expression data
            gene_expression_matrix <- list()
            valid_gene_names <- c()
            
            for (gene_name in selected_genes) {
                gene_id <- NULL
                if (!is.null(raw_data$genes)) {
                    gene_match <- raw_data$genes[raw_data$genes$name == gene_name, ]
                    if (nrow(gene_match) > 0) {
                        gene_id <- gene_match$gene[1]
                    }
                }
                
                if (is.null(gene_id) || !(gene_id %in% colnames(full_matrix))) {
                    next
                }
                
                # Get expression values
                if (!is.null(row_idx) && length(row_idx) > 0) {
                    gene_expression <- full_matrix[[gene_id]][row_idx]
                } else {
                    row_indices <- match(rownames(filtered_data), rownames(full_matrix))
                    valid_indices <- !is.na(row_indices)
                    
                    if (sum(valid_indices) == 0) {
                        next
                    }
                    
                    gene_expression <- full_matrix[[gene_id]][row_indices[valid_indices]]
                }
                
                gene_expression_matrix[[gene_name]] <- gene_expression
                valid_gene_names <- c(valid_gene_names, gene_name)
            }
            
            if (length(gene_expression_matrix) == 0) {
                return(create_empty_plot("No data available for selected genes"))
            }
            
            # Calculate expression values
            if (length(valid_gene_names) == 1) {
                avg_expression <- gene_expression_matrix[[1]]
                plot_title <- valid_gene_names[1]
            } else {
                expr_matrix <- do.call(cbind, gene_expression_matrix)
                avg_expression <- rowMeans(expr_matrix, na.rm = TRUE)
                plot_title <- paste("Average of", length(valid_gene_names), "genes:", 
                                   paste(valid_gene_names, collapse = ", "))
            }
            
            # Get group values
            if (!is.null(row_idx) && length(row_idx) > 0) {
                group_values <- filtered_data[[selected_group]]
            } else {
                row_indices <- match(rownames(filtered_data), rownames(full_matrix))
                valid_indices <- !is.na(row_indices)
                group_values <- filtered_data[[selected_group]][valid_indices]
            }
            
            # Ensure lengths match
            if (length(avg_expression) != length(group_values)) {
                min_len <- min(length(avg_expression), length(group_values))
                avg_expression <- avg_expression[1:min_len]
                group_values <- group_values[1:min_len]
            }
            
            # Create plot data
            plot_data <- data.frame(
                gene_expr = avg_expression,
                group = group_values,
                stringsAsFactors = FALSE
            )
            names(plot_data)[2] <- selected_group
            
            plot_data <- plot_data[!is.na(plot_data$gene_expr) & !is.na(plot_data[[selected_group]]), ]
            
            if (nrow(plot_data) == 0) {
                return(create_empty_plot("No data available for selected filters"))
            }
            
            group_col <- tolower(selected_group)
            result <- gene_exp_plot(plot_data, group_col, add_title = plot_title)
            
            return(result)
            
        }, error = function(e) {
            return(create_empty_plot(paste("Error:", e$message)))
        })
    })
}

#' Setup gene link observers
#' @param input Shiny input object
#' @param gene_data Reactive expression for gene data
setup_gene_link_observers <- function(input, gene_data) {
    observeEvent(input$genes_exp_0, {
        req(input$genes_exp_0)
        first_gene <- if (is.character(input$genes_exp_0) && length(input$genes_exp_0) > 0) {
            input$genes_exp_0[1]
        } else {
            input$genes_exp_0
        }
        gene_id <- get_gene_id(first_gene, gene_data()$genes)
        gene_links(gene_id)
    })
}

#' Setup dynamic gene expression panel handlers
#' @param input Shiny input object
#' @param session Shiny session object
#' @param filtered_gene_exp_data Reactive expression for filtered gene expression data
#' @param gene_data Reactive expression for gene data
#' @param global_loading Reactive value for global loading state
#' @param output Shiny output object
setup_dynamic_gene_panels <- function(input, session, filtered_gene_exp_data, 
                                     gene_data, global_loading, output) {
    choice_exp <- reactiveVal()
    counter <- reactiveVal(0)
    
    # Add button observer
    observeEvent(input$click_add_btn, {
        btn_id <- input$click_add_btn
        req(btn_id)
        
        id <- as.numeric(gsub("add_btn_?", "", btn_id))
        
        if (is.na(id)) {
            return(NULL)
        }
        
        shinyjs::hide(paste0("add_btn_", id))

        req(filtered_gene_exp_data())
        choice_exp(isolate(get_gene_choices(filtered_gene_exp_data()$genes, FALSE)))

        # Add new UI block
        if (id == 0) {
            id <- id + 1
            insertUI(
                selector = "#gene_exp_container",
                where = "beforeEnd",
                ui = column(
                    width = 6,
                    id = paste0("dynamic_col_", id),
                    subBlock(id)
                )
            )
            
            # Update selectize input
            selectize_choices <- NULL
            try({
                selectize_choices <- choice_exp()
            }, silent = TRUE)
            
            later(function() {
                tryCatch({
                    if (!is.null(selectize_choices)) {
                        default_selection <- if (length(selectize_choices) > 0) {
                            selectize_choices[1]
                        } else {
                            NULL
                        }
                        
                        updateSelectizeInput(
                            session,
                            inputId = paste0("genes_exp_", id),
                            choices = selectize_choices,
                            selected = default_selection,
                            server = TRUE
                        )
                    }
                }, error = function(e) {})
            }, 0.25)

            # Create render for new plot
            create_gene_exp_render(output, id, input, filtered_gene_exp_data, 
                                  gene_data, global_loading)
        } else {
            id <- id + 1
            newBlock(id, counter, choice_exp(), subBlock, session)
            
            selectize_choices <- NULL
            try({
                selectize_choices <- choice_exp()
            }, silent = TRUE)
            
            later(function() {
                tryCatch({
                    if (!is.null(selectize_choices)) {
                        default_selection <- if (length(selectize_choices) > 0) {
                            selectize_choices[1]
                        } else {
                            NULL
                        }
                        
                        updateSelectizeInput(
                            session,
                            inputId = paste0("genes_exp_", id),
                            choices = selectize_choices,
                            selected = default_selection,
                            server = TRUE
                        )
                    }
                }, error = function(e) {})
            }, 0.25)

            create_gene_exp_render(output, id, input, filtered_gene_exp_data, 
                                  gene_data, global_loading)
        }
    })
    
    # Remove button observer
    observeEvent(input$click_remove_btn, {
        rm_btn_id <- input$click_remove_btn$btn_id
        id <- as.numeric(gsub("remove_btn_", "", rm_btn_id))
        ids <- as.numeric(unlist(input$click_remove_btn$ids))
        
        try({
            output[[paste0("gene_exp_plotly_", id)]] <- NULL
        }, silent = TRUE)

        try({
            removeUI(selector = paste0("#dynamic_col_", id), multiple = FALSE, immediate = TRUE)
        }, silent = TRUE)

        try({
            shinyjs::runjs(sprintf("$('#dynamic_col_%s').remove();", id))
        }, silent = TRUE)
        
        remaining_ids <- ids[ids != id]

        for (i in remaining_ids) {
            shinyjs::hide(paste0("add_btn_", i))
        }

        if (length(remaining_ids) == 0) {
            shinyjs::show("add_btn_0")
        } else {
            shinyjs::show(paste0("add_btn_", max(remaining_ids)))
        }
    }, ignoreNULL = TRUE)
    
    # Toggle button observer
    observeEvent(list(input$click_toggle_exp), {
        req(input$click_toggle_exp)

        tg_id <- input$click_toggle_exp$id
        id <- as.numeric(gsub("check_exp_", "", tg_id))
        is_checked <- input$click_toggle_exp$value

        choice_exp(get_gene_choices(gene_data()$genes, is_checked))

        updateSelectizeInput(
            session,
            paste0("genes_exp_", id),
            label = if (is_checked) "Gene ID" else "Gene name",
            choices = choice_exp(),
            server = TRUE
        )
    })
}

#' Create gene expression render for dynamic panel
#' @param output Shiny output object
#' @param plot_id ID of the plot
#' @param input Shiny input object
#' @param filtered_gene_exp_data Reactive expression for filtered gene expression data
#' @param gene_data Reactive expression for gene data
#' @param global_loading Reactive value for global loading state
create_gene_exp_render <- function(output, plot_id, input, filtered_gene_exp_data, 
                                   gene_data, global_loading) {
    output[[paste0("gene_exp_plotly_", plot_id)]] <- local({
        p_id <- plot_id
        renderPlotly({
            req(
                gene_data(),
                input[[paste0("genes_exp_", p_id)]],
                input[[paste0("group_exp_", p_id)]]
            )

            if (global_loading()) {
                return(NULL)
            }

            tryCatch({
                fg <- filtered_gene_exp_data()
                cur_genes <- isolate(input[[paste0("genes_exp_", p_id)]])
                cur_group <- isolate(input[[paste0("group_exp_", p_id)]])
                cur_check <- isolate(input[[paste0("check_exp_", p_id)]])

                if (is.null(cur_genes) || length(cur_genes) == 0) {
                    return(create_empty_plot("No genes selected"))
                }
                
                # Convert gene names to IDs
                gene_ids <- c()
                if (isTRUE(cur_check)) {
                    gene_ids <- cur_genes
                } else {
                    for (gene_name in cur_genes) {
                        gene_id <- get_gene_id(gene_name, fg$genes)
                        if (!is.na(gene_id)) {
                            gene_ids <- c(gene_ids, gene_id)
                        }
                    }
                }
                
                if (length(gene_ids) == 0) {
                    return(create_empty_plot("No valid genes found"))
                }

                static_df <- tryCatch({
                    build_plot_slim(fg$data, fg$full, gene_ids, cur_group)
                }, error = function(e) {
                    NULL
                })

                if (is.null(static_df) || nrow(static_df) == 0) {
                    return(create_empty_plot("No data"))
                }
                
                plot_title <- if (length(gene_ids) == 1) {
                    cur_genes[1]
                } else {
                    paste("Average of", length(gene_ids), "genes:", paste(cur_genes, collapse = ", "))
                }

                gene_exp_plot(static_df, tolower(cur_group), add_title = plot_title)
            }, error = function(e) {
                return(NULL)
            })
        })
    })
}
