# Analysis section: the tab set holding all tabs (ui/tab_*.R, ui/about.R).

ui_analysis_section <- function() {
  div(
    id = "analysis_section",
    div(id = "particles-js-analysis", class = "particles-js-full"),
    div(
      class = "analysis-dashboard",
      # Main Content for Analysis
      div(
        class = "analysis-content-wrapper",
        div(
          # filters-folded: the Filters cards are folded (www/script.js)
          class = "analysis-content w-100 filters-folded",
          style = "overflow: visible; position: relative; z-index: 1;",
          name_tab_panes(tabsetPanel(
            id = "tabset",

            ui_tab_embeddings(),
            ui_tab_gene_expression(),
            ui_tab_plots(),
            ui_tab_data_table(),
            ui_tab_qc(),
            ui_tab_data_sources(),
            code_link_tab("Code", "source_code"),
            ui_tab_manual(),
            ui_tab_about()
          ))
        )
      )
    )
  )
}

# The tab set with its panes named after the tab titles ("tab-embeddings",
# "tab-gene-expression", ...) rather than numbered, so that the links of the
# tabs read as what they open.
name_tab_panes <- function(tabset) {
  query <- htmltools::tagQuery(tabset)
  is_tab_link <- function(tag) identical(tag$attribs[["data-toggle"]], "tab")
  for (link in Filter(is_tab_link, query$find("a")$selectedTags())) {
    pane <- sub("^#", "", link$attribs$href)
    name <- paste0("tab-", tab_slug(link$attribs[["data-value"]]))
    query$find(paste0("#", pane))$removeAttrs("id")$addAttrs(id = name)
    query$find("a")$
      filter(function(tag, i) identical(tag$attribs$href, paste0("#", pane)))$
      removeAttrs("href")$addAttrs(href = paste0("#", name))
  }
  query$allTags()
}

# "Gene expression" -> "gene-expression"
tab_slug <- function(title) {
  gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(title)))
}
