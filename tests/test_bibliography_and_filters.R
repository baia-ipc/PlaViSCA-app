# Bibliography validation and filter-behaviour tests.

# -------------------------------------------------------------- bibliography

test_that_("the bibliography maps exactly onto the six atlas studies", {
    m <- app_data("mr_data")
    b <- app_data("bibliography")
    res <- validate_bibliography(m, b)
    failures <- res[res$status == "FAIL", , drop = FALSE]
    expect_empty_(
        sprintf("%s: %s", failures$check, failures$detail),
        "bibliography validation failures"
    )
})

test_that_("the bibliography has exactly six records, one per study", {
    b <- app_data("bibliography")
    expect_equal_(nrow(b), 6L, "bibliography record count")
    expect_setequal_(as.character(b$Study_label), app_expected_studies(), "bibliography study labels")
})

test_that_("bibliography per-study cell counts agree with the atlas", {
    m <- app_data("mr_data")
    b <- app_data("bibliography")
    atlas <- table(as.character(m[[get_data_column("study_label")]]))
    mism <- character(0)
    for (i in seq_len(nrow(b))) {
        s <- as.character(b$Study_label[i])
        if (as.numeric(b$Number_of_cells[i]) != as.numeric(atlas[[s]])) {
            mism <- c(mism, sprintf("%s: %s vs %s", s, b$Number_of_cells[i], atlas[[s]]))
        }
    }
    expect_empty_(mism, "bibliography/atlas cell-count mismatches")
})

test_that_("each study carries exactly one PMID in the atlas", {
    m <- app_data("mr_data")
    n <- tapply(
        as.character(m[[get_data_column("study_pmid")]]),
        as.character(m[[get_data_column("study_label")]]),
        function(x) length(unique(x))
    )
    expect_empty_(names(n)[n != 1], "studies with more than one PMID")
})

test_that_("a missing or extra bibliography study is detected, not ignored", {
    m <- app_data("mr_data")
    b <- app_data("bibliography")
    broken <- b[-1, , drop = FALSE]
    res <- validate_bibliography(m, broken)
    expect_true_(
        any(res$status == "FAIL"),
        "dropping a bibliography record must produce a FAIL"
    )
    extra <- rbind(b, b[1, , drop = FALSE])
    extra$Study_label[nrow(extra)] <- "NotAStudy2099"
    res2 <- validate_bibliography(m, extra)
    expect_true_(
        any(res2$status == "FAIL"),
        "an extra bibliography record must produce a FAIL"
    )
})

# ------------------------------------------------------------------- filters

test_that_("every configured filter field can populate non-empty choices", {
    m <- app_data("mr_data")
    empty <- character(0)
    for (key in filter_field_keys()) {
        col <- get_data_column(key)
        if (!col %in% colnames(m)) {
            empty <- c(empty, sprintf("%s (column %s absent)", key, col))
            next
        }
        vals <- order_column_values(col, m[[col]])
        if (length(vals) == 0) empty <- c(empty, sprintf("%s (no values)", key))
    }
    expect_empty_(empty, "filter fields that cannot populate choices")
})

test_that_("a valid filter combination returns rows", {
    m <- app_data("mr_data")
    sel <- list()
    sel[[get_data_column("broad_stage")]] <- "Blood stage"
    sel[[get_data_column("detailed_stage")]] <- "Ring/Early trophozoite"
    keep <- rep(TRUE, nrow(m))
    for (col in names(sel)) keep <- keep & m[[col]] %in% sel[[col]]
    expect_true_(sum(keep) > 0, "Blood stage + Ring/Early trophozoite must return rows")
})

test_that_("an impossible filter combination is constrained rather than silently empty", {
    available <- unique(app_data("mr_data")[[get_data_column("detailed_stage")]])
    allowed <- compatible_detailed_stages("Sporozoite stage", available)
    expect_true_(
        !("Ring/Early trophozoite" %in% allowed),
        "Ring/Early trophozoite must not be offered when Sporozoite stage is selected"
    )
    expect_true_(
        !is.null(stage_selection_message("Sporozoite stage", "Ring/Early trophozoite")),
        "an impossible combination must produce an explicit message"
    )
})

test_that_("true NA values are displayed as the configured missing label", {
    m <- app_data("mr_data")
    col <- get_data_column("idc_reference_class")
    disp <- relabel_missing(m[[col]][is.na(m[[col]])][1])
    expect_equal_(disp, MISSING_VALUE_LABEL, "NA display label")
    expect_true_(
        !is.null(MISSING_VALUE_LABEL) && nzchar(MISSING_VALUE_LABEL),
        "a missing label must be configured"
    )
})

test_that_("missing-value relabelling does not mutate the underlying data", {
    m <- app_data("mr_data")
    col <- get_data_column("idc_reference_class")
    before <- sum(is.na(m[[col]]))
    invisible(relabel_missing(m[[col]]))
    after <- sum(is.na(app_data("mr_data")[[col]]))
    expect_equal_(after, before, "NA count must be unchanged after display relabelling")
})

test_that_("numeric-sorted fields sort numerically, not lexicographically", {
    vals <- order_field_values("idc_reference_class", c("32", "6", "48", "12"))
    expect_equal_(vals, c("6", "12", "32", "48"), "IDC class ordering")
})

test_that_("a filter column missing from the data raises an error", {
    m <- app_data("mr_data")[1:10, , drop = FALSE]
    err <- tryCatch({
        if (!exists("count_cells")) skip_test("count_cells not loaded in this context")
        count_cells(m, "a_column_that_does_not_exist")
        NULL
    }, error = function(e) conditionMessage(e))
    if (is.null(err)) fail("a missing plot column must raise an error")
    expect_true_(
        grepl("data error", err, ignore.case = TRUE),
        sprintf("the error must say it is a data error, got: %s", err)
    )
})

test_that_("an excluding filter keeps the cells without the selected values", {
    if (!exists("apply_secondary_filters")) skip_test("filters not loaded in this context")
    m <- app_data("mr_data")
    col <- get_data_column("study_label")
    dropped <- "Hazzard2024"
    inputs <- setNames(list(dropped), col)
    attr(inputs, "exclude") <- col
    out <- apply_secondary_filters(m, inputs)
    expect_true_(!dropped %in% out[[col]], "the excluded study must be gone")
    expect_equal_(nrow(out), sum(m[[col]] != dropped & m$n_feature_rna > 0 & m$n_count_rna > 0),
        "all the other cells must be kept")
    expect_equal_(length(attr(out, "row_idx")), nrow(out), "row indices follow the exclusion")
    attr(inputs, "exclude") <- character(0)
    kept <- apply_secondary_filters(m, inputs)
    expect_true_(all(kept[[col]] == dropped), "without exclude the selection is kept")
})

test_that_("an excluding stage filter constrains the other stage filter", {
    all_broad <- unique(as.character(app_data("mr_data")[[get_data_column("broad_stage")]]))
    effective <- filter_effective_values("Blood stage", TRUE, all_broad)
    expect_true_(!"Blood stage" %in% effective && length(effective) > 0, "exclude gives the other broad stages")
    detailed <- compatible_detailed_stages(effective)
    map <- stage_compatibility_map()
    expect_true_(!any(map[detailed] == "Blood stage"), "no blood stage is offered when blood is excluded")
    expect_true_(is.null(filter_effective_values(character(0), TRUE, all_broad)),
        "an empty selection is no constraint, also when excluding")
})
