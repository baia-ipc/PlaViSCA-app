# Embedding Visualization Module
# Functions for handling embedding plots (PCA, UMAP, t-SNE)

#' Setup embedding gene UI observers
#' @param input Shiny input object
#' @param session Shiny session object
#' @param gene_data Reactive expression for gene data
#' @param genes_loading Reactive value for gene loading state
setup_embedding_gene_ui <- function(input, session, gene_data, genes_loading) {
    # Dynamic UI for gene selection
    shinyjs::hide("gene_snn_ui")
    shinyjs::hide("data_type")
    
    observe({
        req(input$color)
        
        if (input$color == "genes") {
            shinyjs::show("data_type")
            shinyjs::show("gene_snn_ui")
            shinyjs::show("gene_link_ui")
            shinyjs::show("switch_id")

            genes_loading(TRUE)
            
            later(function() {
                genes_loading(FALSE)
            }, 0.5)
        } else {
            shinyjs::hide("gene_snn_ui")
            shinyjs::hide("gene_link_ui")
            shinyjs::hide("data_type")
            shinyjs::hide("switch_id")
            genes_loading(FALSE)
        }
    })

    # Update gene choices when color changes or toggle changes
    observeEvent(list(input$color, input$check), {
        if (!identical(input$color, "genes")) {
            return()
        }
        
        req(gene_data()$genes)

        genes_loading(TRUE)

        tryCatch({
            genes_df <- gene_data()$genes
            if (is.null(genes_df) || nrow(genes_df) == 0) {
                genes_loading(FALSE)
                return()
            }
            
            gene_choices <- get_gene_choices(genes_df, input$check)
            
            if (is.null(gene_choices) || length(gene_choices) == 0) {
                genes_loading(FALSE)
                return()
            }

            current_selection <- input$genes_n

            updateSelectizeInput(
                session,
                "genes_n",
                choices = gene_choices,
                selected = current_selection,
                options = list(
                    placeholder = "Select one or more genes",
                    maxItems = 10
                ),
                server = TRUE
            )
            
            label_text <- if (isTRUE(input$check)) "Gene ID" else "Gene name"
            runjs(sprintf('$("#gene_label").text("%s");', label_text))

            later(function() {
                genes_loading(FALSE)
            }, 0.1)
        }, error = function(e) {
            message("Error updating gene choices: ", e$message)
            genes_loading(FALSE)
        })
    }, ignoreInit = FALSE)
}

#' Setup gene link observers for embedding tab
#' @param input Shiny input object
#' @param gene_data Reactive expression for gene data
setup_embedding_gene_links <- function(input, gene_data) {
    # Gene links for first plot
    observeEvent(input$genes_n, {
        req(input$genes_n)
        
        if (length(input$genes_n) > 1) {
            gene_links_multiple(input$genes_n)
        } else {
            gene_id <- get_gene_id(input$genes_n, gene_data()$genes)
            gene_links(gene_id)
        }
    })
    
    # Gene links for second plot
    observeEvent(input$genes_second, {
        req(input$genes_second)
        
        if (length(input$genes_second) > 1) {
            # For multiple genes, call the multiple link handler
            gene_ids <- sapply(input$genes_second, function(g) {
                get_gene_id(g, gene_data()$genes)
            }, USE.NAMES = FALSE)
            
            # Filter out NA values
            valid_ids <- gene_ids[!is.na(gene_ids)]
            
            if (length(valid_ids) > 0) {
                # Update links for second plot
                shinyjs::runjs(sprintf(
                    'document.getElementById("plasmo_link_second").href = "https://plasmodb.org/plasmo/app/record/gene/%s";',
                    valid_ids[1]
                ))
                shinyjs::runjs(sprintf(
                    'document.getElementById("ncbi_link_second").href = "https://www.ncbi.nlm.nih.gov/protein/%s";',
                    valid_ids[1]
                ))
            }
        } else {
            gene_id <- get_gene_id(input$genes_second, gene_data()$genes)
            # Update second plot links
            shinyjs::runjs(sprintf(
                'document.getElementById("plasmo_link_second").href = "https://plasmodb.org/plasmo/app/record/gene/%s";',
                gene_id
            ))
            shinyjs::runjs(sprintf(
                'document.getElementById("ncbi_link_second").href = "https://www.ncbi.nlm.nih.gov/protein/%s";',
                gene_id
            ))
        }
    })
}

#' Generate embedding plot
#' @param color Color variable
#' @param genes Selected genes
#' @param check Gene ID toggle state
#' @param xaxis X-axis variable
#' @param yaxis Y-axis variable
#' @param size Plot size
#' @param plot Plot type (2d or 3d)
#' @param is_dual_plot Whether this is a dual plot
#' @param data_source Reactive expression for data source
#' @return Plotly object
generate_embedding_plot <- function(color, genes, check, xaxis, yaxis, size, 
                                   plot, is_dual_plot = FALSE, data_source) {
    # Get data
    df_local <- data_source()
    
    if (is.null(df_local) || nrow(df_local) == 0) {
        return(NULL)
    }

    color_column <- NULL

    if (isTRUE(color == "genes")) {
        # Resolve gene input
        gene_inputs <- genes
        gene_data_obj <- tryCatch(gene_data()$genes, error = function(e) NULL)
        full_mat <- if (!is.null(cached_data[[get_current_data_type()]])) {
            cached_data[[get_current_data_type()]]
        } else {
            gene_data()$data
        }
        row_idx <- attr(df_local, "row_idx")

        resolved_gene_col <- NULL
        
        # Handle multiple gene selection
        if (!is.null(gene_inputs) && length(gene_inputs) > 0) {
            if (length(gene_inputs) > 1) {
                # Calculate average expression for multiple genes
                valid_gene_ids <- c()
                
                for (gene_input in gene_inputs) {
                    current_resolved <- NULL
                    
                    if (!is.null(gene_input) && nzchar(as.character(gene_input)) && isTRUE(check)) {
                        candidate <- as.character(gene_input)
                        if (!is.null(full_mat) && candidate %in% colnames(full_mat)) {
                            current_resolved <- candidate
                        }
                    }
                    
                    if (is.null(current_resolved) && !is.null(gene_input) && nzchar(as.character(gene_input))) {
                        mapped <- tryCatch(get_gene_id(as.character(gene_input), gene_data_obj), error = function(e) NULL)
                        if (!is.null(mapped) && !is.na(mapped) && !is.null(full_mat) && mapped %in% colnames(full_mat)) {
                            current_resolved <- as.character(mapped)
                        }
                    }
                    
                    if (is.null(current_resolved) && !is.null(gene_input) && !is.null(full_mat) && 
                        as.character(gene_input) %in% colnames(full_mat)) {
                        current_resolved <- as.character(gene_input)
                    }
                    
                    if (!is.null(current_resolved)) {
                        valid_gene_ids <- c(valid_gene_ids, current_resolved)
                    }
                }
                
                # Calculate average expression
                if (length(valid_gene_ids) > 0) {
                    if (!is.null(row_idx) && length(row_idx) > 0) {
                        expr_values <- sapply(valid_gene_ids, function(gid) full_mat[[gid]][row_idx], simplify = TRUE)
                    } else {
                        row_indices <- match(rownames(df_local), rownames(full_mat))
                        valid_indices <- !is.na(row_indices)
                        expr_values <- sapply(valid_gene_ids, function(gid) {
                            full_mat[[gid]][row_indices[valid_indices]]
                        }, simplify = TRUE)
                    }
                    
                    if (is.matrix(expr_values)) {
                        avg_expr <- rowMeans(expr_values, na.rm = TRUE)
                    } else {
                        avg_expr <- expr_values
                    }
                    
                    resolved_gene_col <- "avg_gene_expression"
                    df_local[[resolved_gene_col]] <- avg_expr
                }
            } else {
                # Single gene
                gene_input <- gene_inputs[1]
                
                if (!is.null(gene_input) && nzchar(as.character(gene_input)) && isTRUE(check)) {
                    candidate <- as.character(gene_input)
                    if (!is.null(full_mat) && candidate %in% colnames(full_mat)) {
                        resolved_gene_col <- candidate
                    }
                }
                
                if (is.null(resolved_gene_col) && !is.null(gene_input) && nzchar(as.character(gene_input))) {
                    mapped <- tryCatch(get_gene_id(as.character(gene_input), gene_data_obj), error = function(e) NULL)
                    if (!is.null(mapped) && !is.na(mapped) && !is.null(full_mat) && mapped %in% colnames(full_mat)) {
                        resolved_gene_col <- as.character(mapped)
                    }
                }
                
                if (is.null(resolved_gene_col) && !is.null(gene_input) && !is.null(full_mat) && 
                    as.character(gene_input) %in% colnames(full_mat)) {
                    resolved_gene_col <- as.character(gene_input)
                }
                
                if (!is.null(resolved_gene_col) && resolved_gene_col %in% colnames(full_mat)) {
                    if (!is.null(row_idx) && length(row_idx) > 0) {
                        df_local[[resolved_gene_col]] <- full_mat[[resolved_gene_col]][row_idx]
                    } else {
                        row_indices <- match(rownames(df_local), rownames(full_mat))
                        valid_indices <- !is.na(row_indices)
                        df_local[[resolved_gene_col]] <- full_mat[[resolved_gene_col]][row_indices[valid_indices]]
                    }
                }
            }
        }
        
        color_column <- resolved_gene_col
    } else {
        color_column <- color
    }

    # Generate plot based on type
    if (plot == "2d") {
        plot_2d(
            df_local,
            xaxis,
            yaxis,
            color_column,
            size,
            is_dual_plot = is_dual_plot
        )
    } else {
        plot_3d(
            df_local,
            xaxis,
            yaxis,
            color_column,
            size,
            is_dual_plot = is_dual_plot
        )
    }
}

#' Setup embedding plot outputs
#' @param output Shiny output object
#' @param input Shiny input object
#' @param first_plot_reactive Reactive expression for first plot parameters
#' @param second_plot_reactive Reactive expression for second plot parameters
#' @param first_plot_data Reactive expression for first plot data
#' @param second_plot_data Reactive expression for second plot data
#' @param data Reactive expression for main data
#' @param embedding_ready Reactive expression for embedding readiness
#' @param all_dependencies_ready Reactive expression for dependency readiness
#' @param gene_data Reactive expression for gene data
#' @param cached_data Reactive values for cached data
#' @param get_current_data_type_fn Function to get current data type
setup_embedding_outputs <- function(output, input, first_plot_reactive, second_plot_reactive,
                                   first_plot_data, second_plot_data, data, embedding_ready,
                                   all_dependencies_ready, gene_data, cached_data, 
                                   get_current_data_type_fn) {
    # First plot output
    output$first_plotly <- renderPlotly({
        plot_params <- first_plot_reactive()
        
        if (is.null(plot_params)) {
            return(NULL)
        }
        
        generate_embedding_plot(
            plot_params$color, 
            plot_params$genes, 
            plot_params$check,
            plot_params$xaxis,
            plot_params$yaxis,
            plot_params$size,
            plot_params$plot,
            is_dual_plot = TRUE, 
            data_source = first_plot_data
        )
    })

    # Second plot output
    output$second_plotly <- renderPlotly({
        plot_params <- second_plot_reactive()
        
        if (is.null(plot_params)) {
            return(NULL)
        }
        
        generate_embedding_plot(
            plot_params$color, 
            plot_params$genes, 
            plot_params$check,
            plot_params$xaxis,
            plot_params$yaxis,
            plot_params$size,
            plot_params$plot,
            is_dual_plot = TRUE, 
            data_source = second_plot_data
        )
    })
    
    # Main plot output (single plot mode)
    output$all_plotly <- renderPlotly({
        req(!isTRUE(input$enable_comparison))
        req(all_dependencies_ready(), embedding_ready())

        if (is.null(data()) || nrow(data()) == 0) {
            return(NULL)
        }

        generate_embedding_plot(
            input$color,
            input$genes_n,
            input$check,
            input$xaxis,
            input$yaxis,
            input$size,
            input$plot,
            is_dual_plot = FALSE,
            data_source = data
        )
    })
}
