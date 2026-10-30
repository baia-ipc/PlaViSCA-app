# The server of the app: each tab and shared part has its own file in
# server/ (sourced in global.R); this function connects them.

server <- function(input, output, session) {
  # expression data of the session (raw or normalized)
  loaded <- data_loading_server(input, session)

  # filter menus, the filter state shared by the tabs, the stage filters;
  # tab_filters[[tab]]() gives the filter values of a tab, debounced
  filter_choices <- filter_choices_server(input, session, loaded$gene_data)
  tab_filters <- shared_filters_server(input, session, filter_choices)
  stage_filters_server(input, output, session)

  # the tabs
  embeddings_tab_server(input, output, session, loaded, tab_filters[["Embeddings"]])
  gene_expression_tab_server(input, output, session, loaded, tab_filters[["Gene expression"]])
  plots_tab_server(input, output, reactive(mr_data), tab_filters[["Metadata plots"]])
  data_table_tab_server(output, tab_filters[["Metadata table"]])
  qc_tab_server(output, tab_filters[["QC Plots"]])
  data_sources_tab_server(input, output)

  # free the memory of the session
  onSessionEnded(function() gc())
}
