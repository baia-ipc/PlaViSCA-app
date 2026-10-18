# Parts of the page shown on every section: <head> and menu bar.

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

# Top menu bar: title, Home / Analysis / About links and organization logos.
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
    ),
    create_org_logos()
  )
}
