# Parts of the page shown on every section: <head>, menu bar and footer.

# Stylesheets, scripts and fonts loaded in the page <head>.
ui_head <- function() {
  tags$head(
    useShinyjs(),
    includeCSS("www/style.css"),
    tags$script(src = "script.js"),
    tags$link(
      rel = "stylesheet",
      href = external_link("jquery_ui_css")
    ),
    tags$link(rel = "preconnect", href = external_link("google_fonts")),
    tags$link(rel = "preconnect", href = external_link("google_fonts_static")),
    tags$link(
      href = external_link("roboto_font_css"),
      rel = "stylesheet"
    ),
    tags$script(src = external_link("jquery_ui_js")),
    tags$script(src = "particles.min.js"),
    tags$script(src = "particles-config.js")
  )
}

# Top menu bar: title and Home / Analysis / About links.
ui_menu_bar <- function() {
  div(
    class = "menu-bar",
    div(
      class = "menu-title",
      h1("PlaViSca"),
      span(tags$i("Plasmodium vivax"), " Single Cell Analysis Platform")
    ),
    div(
      class = "menu-items",
      actionLink(
        "home",
        label = tagList(icon("home"), " Home"),
        class = "menu-link active"
      ),
      actionLink(
        "analysis",
        label = tagList(icon("table"), " Analysis"),
        class = "menu-link"
      ),
      actionLink(
        "about",
        label = tagList(icon("info-circle"), " About"),
        class = "menu-link"
      )
    )
  )
}

# Footer with organization logos, shown on the home page only.
ui_footer <- function() {
  conditionalPanel(
    condition = "input.home || (!input.analysis && !input.about)", # Show footer only on home page
    div(
      class = "footer",
      create_footer_logos(),
      p(class = "copyright", footer_copyright())
    )
  )
}
