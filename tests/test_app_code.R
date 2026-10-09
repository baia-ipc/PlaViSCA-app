# Checks on the app source code (app.R, global.R, ui.R, server.R, functions/,
# config/): no unsupported column names, no missing sourced files, no
# hardcoded data paths.

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

test_that_("global.R sources only files that exist", {
    global_txt <- readLines(file.path(APP_ROOT, "global.R"), warn = FALSE)
    sourced <- regmatches(global_txt, regexpr('"[^"]+\\.R"', global_txt))
    sourced <- gsub('"', "", sourced)
    missing <- sourced[!file.exists(file.path(APP_ROOT, sourced))]
    expect_empty_(missing, "global.R sources non-existent files")
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
