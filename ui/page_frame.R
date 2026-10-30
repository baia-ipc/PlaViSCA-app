# Parts of the page shown on every section: <head>, loading screen and menu
# bar.

# Stylesheets, scripts and fonts loaded in the page <head>.
ui_head <- function() {
  tags$head(
    useShinyjs(),
    includeCSS("www/style.css"),
    tags$script(src = "script.js"),
    tags$script(src = "fetched_plot.js"),
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

# Loading screen: shown as soon as the page arrives, with the number of
# files loaded so far and the time, until the app is connected
# (www/fetched_plot.js removes it).
ui_loading_screen <- function() {
  tagList(
    div(
      id = "app-loading",
      class = "app-loading",
      div(
        class = "app-loading-box",
        div(class = "app-loading-title", "PlaViSCA"),
        div(class = "app-loading-text", "Loading the application"),
        div(class = "app-loading-detail", id = "app-loading-detail", "")
      )
    ),
    tags$script(HTML(paste(
      "(function () {",
      "  var started = Date.now();",
      "  var timer = setInterval(function () {",
      "    var detail = document.getElementById('app-loading-detail');",
      "    if (!detail) { clearInterval(timer); return; }",
      "    var files = performance.getEntriesByType('resource').length;",
      "    var seconds = Math.round((Date.now() - started) / 1000);",
      "    detail.textContent = files + ' files loaded, ' + seconds + ' s';",
      "  }, 250);",
      "})();",
      sep = "\n"
    )))
  )
}

# Top bar: title and organization logos.
ui_menu_bar <- function() {
  div(
    class = "menu-bar",
    div(
      class = "menu-title",
      h1("PlaViSCA"),
      span(tags$i("Plasmodium vivax"), " Single Cell Analysis Platform")
    ),
    create_org_logos()
  )
}
