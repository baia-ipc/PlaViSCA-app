# The filters shared by the tabs.
#
# Every tab with filters has its own copy of the filter inputs (the input ID
# is the field key plus the tab's suffix, e.g. "study_label_qc"). One filter
# state is kept on the server:
#   - the state holds each filter's selection and its "exclude" switch;
#   - only the copy on the tab being viewed writes to it, so that no value
#     travels back from another tab and overwrites a newer one;
#   - a tab takes the shared state when it is opened, so that the hidden tabs
#     are not recomputed at every click;
#   - each tab reads its filters through a debounced reactive, so that a quick
#     series of clicks (e.g. choosing five studies) is computed once.

# Filter input suffix of each tab with filters.
FILTER_TAB_SUFFIXES <- c(
  "Embeddings" = "",
  "Gene expression" = "_gex",
  "Metadata plots" = "_meta",
  "Metadata table" = "_table",
  "QC Plots" = "_qc"
)

# Milliseconds without filter changes before a tab is recomputed.
FILTER_DEBOUNCE_MS <- 800

# Returns, for each tab (named as in FILTER_TAB_SUFFIXES), a reactive with the
# tab's filter values, as collect_filter_inputs() gives them. `filter_choices`
# holds the choices of each filter (field key -> choices), set when the data
# are loaded.
shared_filters_server <- function(input, session, filter_choices) {
  state <- reactiveValues()

  active_suffix <- reactive({
    sfx <- FILTER_TAB_SUFFIXES[input$tabset %||% ""]
    if (is.na(sfx)) NULL else unname(sfx)
  })

  # the copy on the tab being viewed writes to the shared state: the
  # selection (state[[key]]) and the "exclude" switch (state[[exclude_id]])
  for (key in filter_field_keys()) {
    for (sfx in FILTER_TAB_SUFFIXES) {
      local({
        k <- key
        s <- sfx
        id <- paste0(k, s)
        exclude_id <- filter_exclude_id(k, s)
        observeEvent(input[[id]],
          {
            if (identical(isolate(active_suffix()), s)) state[[k]] <- input[[id]]
          },
          ignoreNULL = FALSE,
          ignoreInit = TRUE
        )
        observeEvent(input[[exclude_id]],
          {
            if (identical(isolate(active_suffix()), s)) {
              state[[filter_exclude_id(k)]] <- isTRUE(input[[exclude_id]])
            }
          },
          ignoreInit = TRUE
        )
      })
    }
  }

  # a tab takes the shared state when it is opened
  observeEvent(input$tabset, {
    sfx <- active_suffix()
    req(!is.null(sfx))
    for (key in filter_field_keys()) {
      exclude_id <- filter_exclude_id(key, sfx)
      wanted_exclude <- isTRUE(isolate(state[[filter_exclude_id(key)]]))
      if (!identical(isTRUE(isolate(input[[exclude_id]])), wanted_exclude)) {
        updateCheckboxInput(session, exclude_id, value = wanted_exclude)
      }
      id <- paste0(key, sfx)
      wanted <- isolate(state[[key]]) %||% character(0)
      current <- isolate(input[[id]]) %||% character(0)
      if (!setequal(wanted, current)) {
        choices <- isolate(filter_choices[[key]])
        if (is.null(choices)) {
          updateSelectizeInput(session, id, selected = wanted)
        } else {
          updateSelectizeInput(session, id, choices = choices, selected = wanted, server = TRUE)
        }
      }
    }
  })

  lapply(FILTER_TAB_SUFFIXES, function(sfx) {
    debounce(reactive(collect_filter_inputs(input, sfx)), FILTER_DEBOUNCE_MS)
  })
}
