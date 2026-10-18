# Plot builder of the Plots tab (a Shiny module).
# =============================================================================
# A card where the user chooses the field to count cells by, an optional
# second field to split them by, the plot type and whether to show numbers of
# cells or percentages. Below the plot: the help text of the chosen fields and
# a button to export the counts as CSV.
#
#   plot_builder_ui("builder")
#   plot_builder_server("builder", cells, show_missing)
#
# `cells` is a reactive with the selected cells (metadata rows), and
# `show_missing` a reactive TRUE/FALSE.
# =============================================================================

NO_SPLIT <- "none"

split_by_choices <- function(field) {
    fields <- plot_field_choices()
    c("Nothing" = NO_SPLIT, fields[fields != field])
}

plot_builder_ui <- function(id, field = "study_label", split_by = "broad_stage",
                            type = "stacked", show_as = "percent") {
    ns <- NS(id)
    card(
        full_screen = TRUE,
        class = "plot-builder",
        card_body(
            # Not in the card header: the header styles of the app would
            # break this layout
            layout_column_wrap(
                width = "220px",
                class = "plot-builder-controls",
                selectInput(
                    ns("field"), "Count cells by", plot_field_choices(),
                    selected = field
                ),
                selectInput(
                    ns("split_by"), "Split by", split_by_choices(field),
                    selected = split_by
                ),
                selectInput(
                    ns("type"), "Plot type", plot_types_for(split_by != NO_SPLIT),
                    selected = type
                ),
                radioButtons(
                    ns("show_as"), "Show",
                    c("Cells" = "cells", "% of cells" = "percent"),
                    selected = show_as, inline = TRUE
                )
            ),
            withSpinner(
                type = 6, size = 0.5, color = "#3498db",
                plotlyOutput(ns("plot"), height = "600px")
            )
        ),
        card_footer(
            div(
                class = "d-flex justify-content-between align-items-start gap-3",
                div(class = "text-muted small", uiOutput(ns("help"))),
                downloadButton(ns("download"), "Export .csv", class = "btn btn-success btn-sm")
            )
        )
    )
}

plot_builder_server <- function(id, cells, show_missing) {
    moduleServer(id, function(input, output, session) {
        # A field cannot be split by itself
        observeEvent(input$field, {
            split_by <- input$split_by
            if (identical(split_by, input$field)) split_by <- NO_SPLIT
            updateSelectInput(session, "split_by",
                choices = split_by_choices(input$field), selected = split_by
            )
        }, ignoreInit = TRUE)

        two_fields <- reactive(!is.null(input$split_by) && input$split_by != NO_SPLIT)

        # Plot types that fit one or two fields; keep the current type if it fits
        observeEvent(two_fields(), {
            types <- plot_types_for(two_fields())
            selected <- if (isTRUE(input$type %in% types)) input$type else types[[1]]
            updateSelectInput(session, "type", choices = types, selected = selected)
        }, ignoreInit = TRUE)

        keys <- reactive({
            req(input$field)
            split_by <- if (two_fields() && input$split_by != input$field) input$split_by
            c(input$field, split_by)
        })
        columns <- reactive(vapply(keys(), get_data_column, character(1), USE.NAMES = FALSE))
        file_stem <- reactive(paste0("plavisca_cells_by_", paste(keys(), collapse = "_and_")))

        output$plot <- renderPlotly({
            cols <- columns()
            plot_cell_counts(
                cells(), cols[[1]], if (length(cols) > 1) cols[[2]],
                type = input$type,
                as_percent = identical(input$show_as, "percent"),
                show_missing = show_missing()
            ) %>%
                config(toImageButtonOptions = list(filename = file_stem()))
        })

        output$help <- renderUI({
            lines <- lapply(keys(), function(key) {
                help <- get_data_help(key)
                if (!is.null(help)) tags$div(tags$b(get_data_label(key)), ": ", help)
            })
            tagList(Filter(Negate(is.null), lines))
        })

        output$download <- downloadHandler(
            filename = function() paste0(file_stem(), ".csv"),
            content = function(file) {
                cols <- columns()
                column2 <- if (length(cols) > 1) cols[[2]]
                counts <- count_cells(cells(), cols[[1]], column2, show_missing = show_missing())
                utils::write.csv(
                    count_table_for_export(counts, cols[[1]], column2), file,
                    row.names = FALSE
                )
            }
        )
    })
}
