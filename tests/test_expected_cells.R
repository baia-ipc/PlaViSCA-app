# Exact cell membership and totals of the app data. Update these numbers
# together with the data (and expected_cell_count in config/data_columns.yml).

test_that_("total and per-study cell counts are exact", {
    m <- app_data("mr_data")
    expected <- c(
        "Mancio-Silva2022" = 1480L,
        "Sa2020" = 9766L,
        "Hazzard2022" = 3294L,
        "Hazzard2024" = 80024L,
        "Ruberto2022_1" = 1438L,
        "Ruberto2022_2" = 9947L
    )
    observed <- table(as.character(m[[get_data_column("study_label")]]))
    expect_equal_(nrow(m), 105949L, "total cell count")
    expect_equal_(as.integer(observed[names(expected)]), as.integer(expected),
        "per-study cell counts")
})

test_that_("the 14 duplicated Mancio-Silva2022 cell copies are removed, their partners kept", {
    # See PlaViSca-meta-analysis/metadata/mancio_silva2022_duplicate_cells.tsv.
    m <- app_data("mr_data")
    ids <- as.character(m[[get_data_column("barcode")]])
    removed <- c(
        "D1SEQ100_GGTGCCCGGGGG.1", "D1SEQ100_ACTCATGAAACG.1",
        "D1SEQ100_CCGTCGCTAACA.1", "D1SEQ100_TGTGCTGAGTGA.1",
        "D1SEQ100_TTAGCCAAGGAT.1", "D1SEQ100_TGCCAGTTTCCA.1",
        "D1SEQ100_GGAATCACCACA.1", "D1SEQ100_CAGAGCCGGGGT.1",
        "D1SEQ100_GTAACATTCTTA.1", "D5SEQ50_CAAAGGATGTCG.1",
        "D5SEQ100_TCTTCGAGATCC.1", "D11SEQ100_ATCTCGCAGCGA.1",
        "D14SEQ100_TCCGCGAGTAAG.1", "D4Seq1_GCTGAATTTCGG.1"
    )
    retained <- sub("\\.1$", "", removed)
    expect_empty_(intersect(removed, ids), "duplicate copies still present")
    expect_empty_(setdiff(retained, ids), "partner cells missing")
})

test_that_("both D5 cells with identical counts from different runs are kept, unflagged", {
    m <- app_data("mr_data")
    pair <- c("D5Seq1_CCCCGATTGACG", "D5Seq2_CCCCGATTGACG")
    rows <- m[match(pair, as.character(m[[get_data_column("barcode")]])), , drop = FALSE]
    expect_true_(all(!is.na(rows[[get_data_column("barcode")]])), "both D5 cells must exist")
    expect_equal_(length(unique(as.character(rows$run_id))), 2L,
        "the two D5 cells must keep their distinct source runs")
    suspect_cols <- grep("duplic|suspic", colnames(m), ignore.case = TRUE, value = TRUE)
    expect_empty_(suspect_cols, "unexpected duplicate/suspicion columns in app-facing data")
})

test_that_("exactly seven low-UMI Hazzard2024 cells have paired missing stages", {
    m <- app_data("mr_data")
    broad <- is.na(m[[get_data_column("broad_stage")]])
    detail <- is.na(m[[get_data_column("detailed_stage")]])
    expect_true_(identical(broad, detail), "broad and detailed missing-stage masks")
    expect_equal_(sum(broad), 7L, "paired missing-stage count")
    expect_true_(all(m$study_label[broad] == "Hazzard2024"),
        "missing-stage cells must all be Hazzard2024")
    expect_true_(all(m$total_umi_count[broad] < 10),
        "missing-stage cells must all be below the 10-UMI minimum for stage inference")
    expect_equal_(relabel_missing(NA_character_), MISSING_VALUE_LABEL,
        "missing-stage display label")
})

test_that_("IDC and gametocyte totals are exact and gametocytes are blood-stage only", {
    m <- app_data("mr_data")
    idc <- m[[get_data_column("idc_reference_class")]]
    gam <- m[[get_data_column("pred_gametocyte_sex")]]
    expect_equal_(sum(!is.na(idc)), 90219L, "IDC labeled count")
    expect_equal_(sum(!is.na(gam)), 24166L, "gametocyte call count")
    det <- m[[get_data_column("detailed_stage")]]
    expect_equal_(sum(det == "Female gametocyte", na.rm = TRUE), 11391L, "Female gametocyte count")
    expect_equal_(sum(det == "Male gametocyte", na.rm = TRUE), 2931L, "Male gametocyte count")
    expect_equal_(sum(m[[get_data_column("broad_stage")]] != "Blood stage" & !is.na(gam), na.rm = TRUE),
        0L, "non-blood gametocyte calls")
})

test_that_("metadata, raw and normalized tables have the same sorted cell keys", {
    m <- app_data("mr_data")
    n <- app_data("norm_df")
    r <- app_data("raw_df")
    keys <- list(rownames(m), rownames(n), rownames(r))
    expect_true_(all(vapply(keys, length, integer(1)) == 105949L),
        "all three tables must have 105949 rows")
    expect_true_(all(vapply(keys, function(x) !anyDuplicated(x), logical(1))),
        "all three tables must have unique keys")
    expect_true_(all(vapply(keys[-1], identical, logical(1), keys[[1]])),
        "all three tables must have identical key order")
    expect_true_(identical(keys[[1]], sort(keys[[1]])),
        "cell-key order must be canonical lexicographic order")
})
