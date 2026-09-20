# Acceptance tests AT19 and AT20.
# =============================================================================
# AT19: every configured app field has a valid current-schema target, and the
#       candidate data actually contains that target.
# AT20: no live app code relies on the obsolete stage/HPI/geography vocabulary.
# =============================================================================

# ------------------------------------------------------------------- AT19

test_that_("AT19: every configured field resolves to a real current-schema column", {
    m <- app_data("mr_data")
    unresolved <- character(0)
    for (key in data_field_keys()) {
        col <- get_data_column(key)
        if (is.null(col) || !nzchar(col)) {
            unresolved <- c(unresolved, sprintf("%s (no target)", key))
        } else if (!col %in% colnames(m)) {
            unresolved <- c(unresolved, sprintf("%s -> %s", key, col))
        }
    }
    expect_empty_(unresolved, "AT19 violations: configured fields with no valid target")
})

test_that_("AT19: every configured field also exists in the expression tables", {
    n <- app_data("norm_df")
    missing <- character(0)
    for (key in data_field_keys()) {
        col <- get_data_column(key)
        if (!col %in% colnames(n)) missing <- c(missing, sprintf("%s -> %s", key, col))
    }
    expect_empty_(missing, "configured fields absent from normalize_df")
})

test_that_("AT19: every field referenced by the filter/category/detail lists is configured", {
    known <- data_field_keys()
    unknown <- setdiff(
        unique(c(filter_field_keys(), category_field_keys(), detail_table_field_keys())),
        known
    )
    expect_empty_(unknown, "field keys used by the UI but not declared in the config")
})

# ------------------------------------------------------------------- AT20

# Obsolete physical metadata names that must not appear in live app code.
AT20_OBSOLETE_PATTERNS <- c(
    "development_phase",
    "parasite_stages",
    "parasite_stage\\b",
    "life_cycle_stage",      # bare; the allowed names are prefixed (see below)
    "hour_post_invasion",
    "geographic_location",
    "blood_stage"
)

# Current-schema names that legitimately contain an obsolete substring.
AT20_ALLOWED <- c(
    "harmonized_life_cycle_stage",
    "source_life_cycle_stage",
    "parasite_broad_stage",
    "parasite_lineage_isolate",
    "parasite_origin_location"
)

#' Scan live app files for obsolete schema tokens.
#'
#' Comment lines are ignored (a comment explaining *why* a name is gone is not
#' a code dependency), but no live file is whitelisted: any executable
#' reference is a violation.
scan_obsolete_schema <- function(files = live_app_files()) {
    hits <- list()
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        for (i in seq_along(lines)) {
            line <- lines[i]
            stripped <- trimws(line)
            if (startsWith(stripped, "#")) next
            # blank out the allowed current-schema names before matching
            probe <- line
            for (ok in AT20_ALLOWED) probe <- gsub(ok, "", probe, fixed = TRUE)
            for (pat in AT20_OBSOLETE_PATTERNS) {
                if (grepl(pat, probe)) {
                    hits[[length(hits) + 1]] <- data.frame(
                        file = sub(paste0("^", APP_ROOT, "/?"), "", f),
                        line = i,
                        pattern = pat,
                        text = trimws(line),
                        stringsAsFactors = FALSE
                    )
                }
            }
        }
    }
    if (length(hits) == 0) {
        return(data.frame(
            file = character(0), line = integer(0),
            pattern = character(0), text = character(0),
            stringsAsFactors = FALSE
        ))
    }
    do.call(rbind, hits)
}

test_that_("AT20: no live app code references obsolete stage/HPI/geography columns", {
    # config/data_columns.yml legitimately lists the obsolete names in its
    # `obsolete_columns:` block (that list is what the validator checks
    # against), and functions/data_contract.R asserts hour_post_invasion is
    # NOT required. Those are the two declared, auditable exceptions.
    hits <- scan_obsolete_schema()
    hits <- hits[!(
        (basename(hits$file) == "data_columns.yml") |
            (basename(hits$file) == "data_contract.R") |
            (basename(hits$file) == "test_at19_at20.R")
    ), , drop = FALSE]
    expect_empty_(
        sprintf("%s:%s [%s] %s", hits$file, hits$line, hits$pattern, hits$text),
        "AT20 violations"
    )
})

test_that_("AT20: no stale preprocessing generator remains under functions/", {
    live <- list.files(file.path(APP_ROOT, "functions"), full.names = TRUE)
    offenders <- grep("preprocess", basename(live), ignore.case = TRUE, value = TRUE)
    expect_empty_(offenders, "preprocessing scripts still living in the sourced functions/ directory")
})

test_that_("AT20: legacy files are documented as historical and are not sourced", {
    legacy_dir <- file.path(APP_ROOT, "docs", "legacy")
    if (!dir.exists(legacy_dir)) skip_test("no docs/legacy directory")
    legacy <- list.files(legacy_dir, pattern = "\\.R$", full.names = TRUE)
    expect_true_(length(legacy) > 0, "expected legacy files to have been moved")
    for (f in legacy) {
        head_txt <- paste(readLines(f, n = 20, warn = FALSE), collapse = "\n")
        expect_true_(
            grepl("HISTORICAL", head_txt) && grepl("NOT USED", head_txt),
            sprintf("%s must carry the historical/not-used header", basename(f))
        )
    }
    global_txt <- readLines(file.path(APP_ROOT, "global.R"), warn = FALSE)
    sourced <- grep("^\\s*source\\(", global_txt, value = TRUE)
    expect_empty_(
        grep("docs/legacy", sourced, value = TRUE),
        "global.R must not source anything from docs/legacy"
    )
})

test_that_("AT20: global.R sources only files that exist", {
    global_txt <- readLines(file.path(APP_ROOT, "global.R"), warn = FALSE)
    sourced <- regmatches(global_txt, regexpr('"[^"]+\\.R"', global_txt))
    sourced <- gsub('"', "", sourced)
    missing <- sourced[!file.exists(file.path(APP_ROOT, sourced))]
    expect_empty_(missing, "global.R sources non-existent files")
})

test_that_("AT20: no absolute machine-specific path is hardcoded in app code", {
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

test_that_("AT20: no hardcoded 'data/...' literal remains outside the path helper", {
    offenders <- character(0)
    for (f in live_app_files()) {
        if (basename(f) == "data_contract.R") next
        lines <- readLines(f, warn = FALSE)
        lines <- lines[!startsWith(trimws(lines), "#")]
        hit <- grep('"data/[^"]+"', lines, value = TRUE)
        if (length(hit) > 0) offenders <- c(offenders, sprintf("%s: %s", basename(f), hit))
    }
    expect_empty_(offenders, "hardcoded data/ paths outside app_data_path()")
})
