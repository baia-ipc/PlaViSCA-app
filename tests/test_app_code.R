# Checks on the app source code (app.R, global.R, ui.R, server.R, functions/,
# ui/, config/, www/script.js): no unsupported column names, no missing sourced
# files, no hardcoded data paths or external URLs, valid site configuration.

#' Scan app source files for the unsupported column names listed in
#' config/data_columns.yml. Names are matched as whole identifiers, so e.g.
#' "parasite_stage" does not match "parasite_stage_source". Comment lines are
#' ignored: a comment mentioning a name is not a code dependency.
scan_unsupported_columns <- function(files = live_app_files()) {
    patterns <- paste0("(?<![A-Za-z0-9_])", app_unsupported_columns(), "(?![A-Za-z0-9_])")
    names(patterns) <- app_unsupported_columns()
    hits <- character(0)
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        for (i in seq_along(lines)) {
            if (startsWith(trimws(lines[i]), "#")) next
            for (name in names(patterns)) {
                if (grepl(patterns[[name]], lines[i], perl = TRUE)) {
                    hits <- c(hits, sprintf(
                        "%s:%d [%s] %s",
                        sub(paste0("^", APP_ROOT, "/?"), "", f), i, name, trimws(lines[i])
                    ))
                }
            }
        }
    }
    hits
}

test_that_("no app code uses an unsupported column name", {
    # Two files name them on purpose: data_columns.yml lists them, and
    # data_checks.R checks that hour_post_invasion is not required.
    files <- live_app_files()
    files <- files[!basename(files) %in% c("data_columns.yml", "data_checks.R")]
    expect_empty_(scan_unsupported_columns(files), "unsupported column names in app code")
})

test_that_("functions/ holds no preprocessing scripts", {
    live <- list.files(file.path(APP_ROOT, "functions"), full.names = TRUE)
    offenders <- grep("preprocess", basename(live), ignore.case = TRUE, value = TRUE)
    expect_empty_(offenders, "preprocessing scripts in the sourced functions/ directory")
})

test_that_("global.R and ui.R source only files that exist", {
    global_txt <- unlist(lapply(file.path(APP_ROOT, c("global.R", "ui.R")), readLines, warn = FALSE))
    sourced <- regmatches(global_txt, regexpr('"[^"]+\\.R"', global_txt))
    sourced <- gsub('"', "", sourced)
    missing <- sourced[!file.exists(file.path(APP_ROOT, sourced))]
    expect_empty_(missing, "sourced files that do not exist")
})

test_that_("every app R file parses", {
    files <- grep("\\.R$", live_app_files(), value = TRUE)
    broken <- Filter(function(f) inherits(try(parse(f), silent = TRUE), "try-error"), files)
    expect_empty_(basename(broken), "R files with syntax errors")
})

test_that_("global.R and ui.R build the page", {
    if (!file.exists(app_data_path("cleaned_dataset.rds"))) {
        skip_test(sprintf("no data at %s", app_data_path("cleaned_dataset.rds")))
    }
    # in a separate R process, as when the app starts: global.R loads the data
    # and the packages, ui.R builds the page from them
    script <- "suppressMessages({library(shiny); source('global.R'); source('ui.R')}); invisible(as.character(ui))"
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(script)),
        stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    expect_true_(is.null(status) || status == 0,
        paste("building the page failed:", paste(tail(out, 5), collapse = " | ")))
})

test_that_("no absolute machine-specific path is hardcoded in app code", {
    offenders <- character(0)
    for (f in live_app_files()) {
        lines <- readLines(f, warn = FALSE)
        lines <- lines[!startsWith(trimws(lines), "#")]
        hit <- grep("/home/[a-z]|/srv/[a-z]|/Users/", lines, value = TRUE)
        if (length(hit) > 0) {
            offenders <- c(offenders, sprintf("%s: %s", basename(f), hit))
        }
    }
    expect_empty_(offenders, "hardcoded absolute paths in application code")
})

test_that_("data file paths are built only in functions/data_paths.R", {
    offenders <- character(0)
    for (f in live_app_files()) {
        if (basename(f) == "data_paths.R") next
        lines <- readLines(f, warn = FALSE)
        lines <- lines[!startsWith(trimws(lines), "#")]
        hit <- grep('"data/[^"]+"', lines, value = TRUE)
        if (length(hit) > 0) offenders <- c(offenders, sprintf("%s: %s", basename(f), hit))
    }
    expect_empty_(offenders, "hardcoded data/ paths outside app_data_path()")
})

test_that_("external URLs are written only in config/external_links.yml and config/team.yml", {
    files <- c(live_app_files(), file.path(APP_ROOT, "www", "script.js"))
    files <- files[file.exists(files) & basename(dirname(files)) != "config"]
    offenders <- character(0)
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        code <- !grepl("^\\s*(#|//)", lines)
        hit <- which(code & grepl("https?://", lines))
        if (length(hit) > 0) {
            offenders <- c(offenders, sprintf("%s:%d %s", basename(f), hit, trimws(lines[hit])))
        }
    }
    expect_empty_(offenders, "hardcoded external URLs")
})

test_that_("external links and team information are complete", {
    expect_equal_(
        external_link("plasmodb_gene", c("PVP01_A", "PVP01_B")),
        c("https://plasmodb.org/plasmo/app/record/gene/PVP01_A",
          "https://plasmodb.org/plasmo/app/record/gene/PVP01_B"),
        "gene link template"
    )
    team <- team_info()
    missing <- setdiff(c("name", "role", "affiliation", "email"), names(team$contact))
    expect_empty_(missing, "fields missing from the contact in team.yml")
    missing <- setdiff(c("authors", "title", "status"), names(team$citation))
    expect_empty_(missing, "fields missing from the citation in team.yml")
    no_logo <- Filter(function(o) is.null(o$logo) || !file.exists(file.path(APP_ROOT, "www", o$logo)),
        team$organizations)
    expect_empty_(vapply(no_logo, function(o) o$name %||% "?", character(1)),
        "organizations whose logo is missing from www/")
})
