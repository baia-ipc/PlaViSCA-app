# Choices of the filter menus, and the coupling of the broad and detailed
# life-cycle stage filters.

# Fills the filter menus of every tab when the data are loaded. The choices
# come from config/data_columns.yml: for every filter field, the values of its
# column in the order of the documented vocabulary. A configured column that
# is missing from the data is reported as an error, not silently skipped.
# Returns the choices of each filter (reactiveValues, field key -> choices).
filter_choices_server <- function(input, session, gene_data) {
  filter_choices <- reactiveValues()

  observeEvent(gene_data(), {
    req(gene_data()$metadata)

    filt_data <- gene_data()$metadata
    missing_columns <- character(0)

    for (key in filter_field_keys()) {
      col <- get_data_column(key)
      if (!col %in% colnames(filt_data)) {
        missing_columns <- c(missing_columns, sprintf("%s -> %s", key, col))
        next
      }
      values <- order_column_values(col, filt_data[[col]])
      choices <- setNames(as.list(values), display_values(col, values))
      if (col %in% italicised_columns()) {
        names(choices) <- format_species_name(values)
      }
      filter_choices[[key]] <- choices

      for (suffix in FILTER_TAB_SUFFIXES) {
        updateSelectizeInput(
          session,
          paste0(key, suffix),
          choices = choices,
          selected = isolate(input[[paste0(key, suffix)]]),
          server = TRUE
        )
      }
    }

    if (length(missing_columns) > 0) {
      showNotification(
        paste0(
          "Data error: these filter fields are configured but missing from ",
          "the loaded data: ", paste(missing_columns, collapse = ", "),
          ". Filters for these fields are unavailable - the displayed data is ",
          "NOT filtered by them."
        ),
        type = "error", duration = NULL
      )
      warning(sprintf(
        "PlaViSCA filter population: missing configured columns: %s",
        paste(missing_columns, collapse = ", ")
      ))
    }
  })

  filter_choices
}

# Broad and detailed stage are not independent: every detailed stage maps to
# exactly one broad stage (config/data_columns.yml `stage_compatibility`).
# On every tab, the broad-stage selection limits the detailed-stage choices,
# and an impossible combination (e.g. from a restored session) is reported
# instead of silently giving no cells. With a filter's "exclude" switch on,
# its effective selection is the other values (filter_effective_values()).
stage_filters_server <- function(input, output, session) {
  all_broad <- unique(as.character(mr_data[[get_data_column("broad_stage")]]))
  all_detailed <- unique(as.character(mr_data[[get_data_column("detailed_stage")]]))

  lapply(unname(FILTER_TAB_SUFFIXES), function(sfx) {
    broad_id <- paste0("broad_stage", sfx)
    detailed_id <- paste0("detailed_stage", sfx)
    broad_exclude_id <- filter_exclude_id("broad_stage", sfx)
    detailed_exclude_id <- filter_exclude_id("detailed_stage", sfx)
    warn_id <- paste0("stage_compatibility_warning", sfx)
    effective_broad <- function() {
      filter_effective_values(input[[broad_id]], input[[broad_exclude_id]], all_broad)
    }

    observeEvent(list(input[[broad_id]], input[[broad_exclude_id]]),
      {
        available <- unique(mr_data[[get_data_column("detailed_stage")]])
        allowed <- compatible_detailed_stages(effective_broad(), available)
        keep <- intersect(isolate(input[[detailed_id]]), allowed)
        updateSelectizeInput(
          session, detailed_id,
          choices = setNames(as.list(allowed), allowed),
          selected = keep, server = TRUE
        )
      },
      ignoreNULL = FALSE, ignoreInit = TRUE
    )

    observe({
      msg <- stage_selection_message(
        effective_broad(),
        filter_effective_values(input[[detailed_id]], input[[detailed_exclude_id]], all_detailed)
      )
      output[[paste0("stage_compatibility_message", sfx)]] <- renderUI({
        if (is.null(msg)) NULL else tagList(fontawesome::fa("triangle-exclamation"), " ", msg)
      })
      if (is.null(msg)) {
        shinyjs::hide(warn_id)
      } else {
        shinyjs::show(warn_id)
      }
    })
  })
  invisible(NULL)
}
