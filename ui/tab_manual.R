# Manual tab: the user manual (manual/docs/build/html), shown inside the app.

ui_tab_manual <- function() {
  tabPanel(
    tagList(fontawesome::fa("book"), " Manual"),
    value = "Manual",
    tags$iframe(
      src = "manual/index.html",
      class = "manual-frame",
      title = "PlaViSca user manual"
    )
  )
}

# Tab-bar link to a GitHub repository, opened in a new browser tab.
code_link_tab <- function(title, link_key) {
  bslib::nav_item(
    tags$a(
      href = external_link(link_key),
      target = "_blank",
      fontawesome::fa("github"), " ", title, " ",
      fontawesome::fa("arrow-up-right-from-square", height = "10px")
    )
  )
}
