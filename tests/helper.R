# Minimal test harness for the PlaViSca app.
# =============================================================================
# Deliberately dependency-free (no testthat): not every environment the app
# runs in has a test framework, and the checks here are simple assertions
# about the app code and data. Run with tests/run_tests.R.
#
# Data tests read PLAVISCA_DATA_DIR exactly like the app does.
# =============================================================================

.TEST_RESULTS <- new.env(parent = emptyenv())
.TEST_RESULTS$rows <- list()
.TEST_RESULTS$current_file <- NA_character_

test_that_ <- function(name, expr) {
    res <- tryCatch(
        {
            force(expr)
            list(status = "PASS", detail = "")
        },
        skip = function(s) list(status = "SKIP", detail = conditionMessage(s)),
        error = function(e) list(status = "FAIL", detail = conditionMessage(e))
    )
    .TEST_RESULTS$rows[[length(.TEST_RESULTS$rows) + 1]] <- data.frame(
        file = .TEST_RESULTS$current_file,
        test = name,
        status = res$status,
        detail = res$detail,
        stringsAsFactors = FALSE
    )
    cat(sprintf("  [%-4s] %s%s\n", res$status, name,
        if (nzchar(res$detail)) paste0(" - ", res$detail) else ""))
    invisible(res$status)
}

skip_test <- function(msg) {
    cond <- structure(
        class = c("skip", "condition"),
        list(message = msg, call = NULL)
    )
    stop(cond)
}

fail <- function(...) stop(paste0(...), call. = FALSE)

expect_true_ <- function(cond, msg) {
    if (!isTRUE(cond)) fail(msg)
    invisible(TRUE)
}

expect_equal_ <- function(actual, expected, msg) {
    if (!isTRUE(all.equal(actual, expected))) {
        fail(sprintf("%s (expected %s, got %s)", msg,
            paste(utils::head(as.character(expected), 8), collapse = ","),
            paste(utils::head(as.character(actual), 8), collapse = ",")))
    }
    invisible(TRUE)
}

expect_setequal_ <- function(actual, expected, msg) {
    if (!setequal(actual, expected)) {
        fail(sprintf(
            "%s\n    only in actual: %s\n    only in expected: %s", msg,
            paste(setdiff(actual, expected), collapse = ", "),
            paste(setdiff(expected, actual), collapse = ", ")
        ))
    }
    invisible(TRUE)
}

expect_empty_ <- function(x, msg) {
    if (length(x) > 0) {
        fail(sprintf("%s: %s", msg, paste(utils::head(as.character(x), 12), collapse = ", ")))
    }
    invisible(TRUE)
}

# ---------------------------------------------------------------- app loading

`%||%` <- function(a, b) if (is.null(a)) b else a

APP_ROOT <- normalizePath(
    Sys.getenv("PLAVISCA_APP_ROOT", unset = file.path(dirname(sys.frame(1)$ofile %||% "."), "..")),
    mustWork = FALSE
)
if (!file.exists(file.path(APP_ROOT, "config", "data_columns.yml"))) {
    APP_ROOT <- normalizePath(getwd(), mustWork = FALSE)
}

load_app_config <- function() {
    old <- setwd(APP_ROOT)
    on.exit(setwd(old), add = TRUE)
    source(file.path(APP_ROOT, "functions", "data_columns_config.R"), local = FALSE)
    source(file.path(APP_ROOT, "functions", "data_paths.R"), local = FALSE)
    source(file.path(APP_ROOT, "functions", "data_checks.R"), local = FALSE)
    invisible(TRUE)
}

.DATA_CACHE <- new.env(parent = emptyenv())

#' Load a data object once per test run.
app_data <- function(which = c("mr_data", "norm_df", "raw_df", "bibliography")) {
    which <- match.arg(which)
    if (!is.null(.DATA_CACHE[[which]])) {
        return(.DATA_CACHE[[which]])
    }
    obj <- switch(which,
        mr_data = {
            p <- app_data_path("cleaned_dataset.rds")
            if (!file.exists(p)) skip_test(sprintf("no data at %s", p))
            readRDS(p)$mr_data
        },
        norm_df = {
            p <- app_data_path("normalize_df.rds")
            if (!file.exists(p)) skip_test(sprintf("no data at %s", p))
            readRDS(p)
        },
        raw_df = {
            p <- app_data_path("raw_df.rds")
            if (!file.exists(p)) skip_test(sprintf("no data at %s", p))
            readRDS(p)
        },
        bibliography = {
            p <- app_data_path("data_source.csv")
            if (!file.exists(p)) skip_test(sprintf("no bibliography at %s", p))
            utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
        }
    )
    .DATA_CACHE[[which]] <- obj
    obj
}

#' Live app source files - the ones that actually run.
live_app_files <- function() {
    files <- c(
        file.path(APP_ROOT, c("app.R", "global.R", "ui.R", "server.R")),
        list.files(file.path(APP_ROOT, "functions"), pattern = "\\.R$", full.names = TRUE),
        list.files(file.path(APP_ROOT, "config"), pattern = "\\.ya?ml$", full.names = TRUE)
    )
    files[file.exists(files)]
}
