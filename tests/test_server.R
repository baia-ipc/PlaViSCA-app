# Checks of the server code: tests/server_checks.R runs the checks of
# tests/server/ with shiny::testServer() in a separate R process (global.R
# loads the data); each check is reported here as a test.

if (!file.exists(app_data_path("cleaned_dataset.rds"))) {
    test_that_("server checks", skip_test(sprintf("no data at %s", app_data_path("cleaned_dataset.rds"))))
} else {
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), "tests/server_checks.R",
        stdout = TRUE, stderr = TRUE))
    results <- grep("^(PASS|FAIL) ", out, value = TRUE)
    test_that_("server checks run", {
        expect_true_(length(results) > 0,
            paste("no results:", paste(tail(out, 5), collapse = " | ")))
    })
    for (line in results) {
        local({
            failed <- startsWith(line, "FAIL ")
            name <- trimws(sub("^(PASS|FAIL) ", "", if (failed) sub(":.*$", "", line) else line))
            detail <- if (failed) sub("^[^:]*: ", "", line) else ""
            test_that_(name, expect_true_(!failed, detail))
        })
    }
}
