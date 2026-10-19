# Plot builder of the Metadata plots tab (a Shiny module).
# =============================================================================
# A card where the user chooses the plot type, then the field to count cells
# by and, for the plot types of two fields, a second field to split them by.
# Below the plot: the help text of the chosen fields, the choice between
# numbers of cells and percentages, a button to export the counts as CSV, and
# the "-" / "+" buttons that remove this plot or add another one.
#
#   metadata_plot_column("plot_builder_1")
#   handles <- plot_builder_server("plot_builder_1", cells, show_missing)
#
# `cells` is a reactive with the selected cells (metadata rows), and
# `show_missing` a reactive TRUE/FALSE. The server returns the "+" and "-"
# button values (`handles$add()`, `handles$remove()`), which the tab server
# (server/tab_plots.R) uses to add and remove plots.
# =============================================================================

# Fields to split by: all fields except the one counted by.
split_by_choices <- function(field) {
    fields <- plot_field_choices()
    fields[fields != field]
}

# The plot builder in a half-width column of the tab's plot row. Its ID is the
# builder ID followed by "_column". `can_remove = FALSE` hides the "-" button.
metadata_plot_column <- function(id, can_remove = TRUE) {
    column(
        width = 6,
        id = paste0(id, "_column"),
        plot_builder_ui(id, can_remove = can_remove)
    )
}

plot_builder_ui <- function(id, type = "stacked", field = "study_label",
                            split_by = "broad_stage", show_as = "percent",
                            can_remove = TRUE) {
    ns <- NS(id)
    split_choices <- split_by_choices(field)
    if (!split_by %in% split_choices) split_by <- split_choices[[1]]
    # The "Split by" menu is shown only for the plot types of two fields
    two_field_condition <- sprintf(
        "[%s].indexOf(input.type) >= 0",
        paste0("'", PLOT_TYPES_TWO_FIELDS, "'", collapse = ", ")
    )

    # No full-screen button: it would cover the "+" button
    card(
        class = "plot-builder shadow-sm mb-3",
        card_body(
            padding = "10px",
            # Not in the card header: the header styles of the app would
            # break this layout
            fluidRow(
                class = "g-2 plot-builder-controls",
                column(
                    width = 4,
                    selectInput(ns("type"), "Plot type", PLOT_TYPE_CHOICES,
                                selected = type, width = "100%")
                ),
                column(
                    width = 4,
                    selectInput(ns("field"), "Count by", plot_field_choices(),
                                selected = field, width = "100%")
                ),
                column(
                    width = 4,
                    conditionalPanel(
                        two_field_condition, ns = ns,
                        selectInput(ns("split_by"), "Split by", split_choices,
                                    selected = split_by, width = "100%")
                    )
                )
            ),
            withSpinner(
                type = 6, size = 0.5, color = "#3498db",
                plotlyOutput(ns("plot"), height = "450px")
            ),
            div(class = "text-muted small mt-2", uiOutput(ns("help")))
        ),
        card_footer(
            div(
                class = "d-flex justify-content-between align-items-center gap-2",
                radioButtons(
                    ns("show_as"), NULL,
                    c("Cells" = "cells", "% of cells" = "percent"),
                    selected = show_as, inline = TRUE
                ),
                div(
                    class = "d-flex gap-2",
                    downloadButton(ns("download"), "Export .csv",
                                   class = "btn-sm btn-outline-secondary"),
                    actionButton(
                        ns("remove"), fontawesome::fa("minus"),
                        class = "btn-sm btn-outline-danger", title = "Remove this plot",
                        style = if (!can_remove) "display: none;"
                    ),
                    actionButton(
                        ns("add"), fontawesome::fa("plus"),
                        class = "btn-sm btn-outline-success", title = "Add a plot"
                    )
                )
            )
        )
    )
}

plot_builder_server <- function(id, cells, show_missing) {
    moduleServer(id, function(input, output, session) {
        # A field cannot be split by itself
        observeEvent(input$field, {
            choices <- split_by_choices(input$field)
            split_by <- input$split_by
            if (!isTRUE(split_by %in% choices)) split_by <- choices[[1]]
            updateSelectInput(session, "split_by", choices = choices, selected = split_by)
        }, ignoreInit = TRUE)

        keys <- reactive({
            req(input$field, input$type)
            split_by <- input$split_by
            if (!is_two_field_type(input$type) || is.null(split_by) ||
                identical(split_by, input$field)) {
                split_by <- NULL
            }
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

        list(add = reactive(input$add), remove = reactive(input$remove))
    })
}
