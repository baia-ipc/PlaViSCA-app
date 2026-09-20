# Life-cycle stage tests: broad/detailed coupling and population gating.

test_that_("every observed detailed stage is in the configured vocabulary", {
    m <- app_data("mr_data")
    observed <- unique(na.omit(as.character(m[[get_data_column("detailed_stage")]])))
    unknown <- setdiff(observed, names(stage_compatibility_map()))
    expect_empty_(unknown, "detailed stages absent from config stage_compatibility")
})

test_that_("every observed broad stage is in the configured vocabulary", {
    m <- app_data("mr_data")
    observed <- unique(na.omit(as.character(m[[get_data_column("broad_stage")]])))
    unknown <- setdiff(observed, get_field_vocabulary("broad_stage"))
    expect_empty_(unknown, "broad stages absent from the configured vocabulary")
})

test_that_("every (detailed, broad) pair in the data matches the documented mapping", {
    m <- app_data("mr_data")
    d <- get_data_column("detailed_stage")
    b <- get_data_column("broad_stage")
    map <- stage_compatibility_map()
    pairs <- unique(m[, c(d, b)])
    pairs <- pairs[!is.na(pairs[[d]]) & !is.na(pairs[[b]]), , drop = FALSE]
    bad <- pairs[map[pairs[[d]]] != pairs[[b]], , drop = FALSE]
    expect_empty_(
        sprintf("%s -> %s", bad[[d]], bad[[b]]),
        "detailed/broad pairs contradicting the documented mapping"
    )
})

test_that_("broad and detailed stage are missing together, never one alone", {
    m <- app_data("mr_data")
    n <- sum(xor(
        is.na(m[[get_data_column("broad_stage")]]),
        is.na(m[[get_data_column("detailed_stage")]])
    ))
    expect_equal_(n, 0L, "cells with only one of the two stage fields missing")
})

test_that_("an impossible broad/detailed combination is reported, not silently empty", {
    expect_true_(
        !stage_selection_is_possible("Liver stage", "Ring"),
        "Liver stage + Ring must be flagged as impossible"
    )
    msg <- stage_selection_message("Liver stage", "Ring")
    expect_true_(!is.null(msg) && nzchar(msg), "an explanatory message must be produced")
})

test_that_("a possible broad/detailed combination is accepted", {
    expect_true_(
        stage_selection_is_possible("Liver stage", "Hypnozoite"),
        "Liver stage + Hypnozoite must be accepted"
    )
    expect_true_(
        is.null(stage_selection_message("Blood stage", c("Ring", "Trophozoite"))),
        "a valid combination must produce no warning"
    )
    expect_true_(
        stage_selection_is_possible(NULL, "Ring"),
        "an empty broad selection means no constraint"
    )
})

test_that_("selecting a broad stage constrains the detailed choices correctly", {
    m <- app_data("mr_data")
    available <- unique(m[[get_data_column("detailed_stage")]])
    liver <- compatible_detailed_stages("Liver stage", available)
    expect_true_(length(liver) > 0, "Liver stage must have compatible detailed stages")
    expect_empty_(
        intersect(liver, c("Ring", "Trophozoite", "Schizont (Blood stage)", "Merozoite")),
        "blood-only stages offered under Liver stage"
    )
    expect_true_(
        "Hypnozoite" %in% liver,
        "Hypnozoite must be offered under Liver stage"
    )
})

test_that_("selecting a detailed stage preserves only compatible broad choices", {
    expect_equal_(compatible_broad_stages("Sporozoite"), "Sporozoite stage",
        "Sporozoite must imply Sporozoite stage")
    expect_equal_(compatible_broad_stages("Hypnozoite"), "Liver stage",
        "Hypnozoite must imply Liver stage")
})

test_that_("detailed stage ordering follows lifecycle progression, not the alphabet", {
    m <- app_data("mr_data")
    ordered <- order_field_values("detailed_stage", m[[get_data_column("detailed_stage")]])
    pos <- function(x) match(x, ordered)
    present <- intersect(c("Sporozoite", "Hypnozoite", "Ring", "Trophozoite",
        "Schizont (Blood stage)", "Merozoite"), ordered)
    if (length(present) >= 2) {
        expect_true_(
            all(diff(pos(present)) > 0),
            sprintf("lifecycle ordering violated: %s", paste(ordered, collapse = " < "))
        )
    }
    expect_true_(
        pos("Sporozoite") < pos("Ring"),
        "Sporozoite must precede blood asexual stages"
    )
})

test_that_("the configured ordering never invents a stage that is absent from the data", {
    m <- app_data("mr_data")
    present <- unique(na.omit(as.character(m[[get_data_column("detailed_stage")]])))
    ordered <- order_field_values("detailed_stage", m[[get_data_column("detailed_stage")]])
    expect_setequal_(ordered, present, "ordered levels vs levels actually present")
})

test_that_("source-defined sporozoites are classified as Sporozoite stage", {
    m <- app_data("mr_data")
    src <- get_data_column("source_life_cycle_stage")
    idx <- which(!is.na(m[[src]]) & m[[src]] == "Sporozoite")
    if (length(idx) == 0) skip_test("no source-defined sporozoites in this candidate")
    expect_setequal_(
        unique(m[[get_data_column("broad_stage")]][idx]), "Sporozoite stage",
        "source-defined sporozoites must be Sporozoite stage"
    )
    expect_setequal_(
        unique(m[[get_data_column("detailed_stage")]][idx]), "Sporozoite",
        "source-defined sporozoites must keep the Sporozoite detailed stage"
    )
})

test_that_("liver-stage cells never expose an inferred gametocyte sex", {
    m <- app_data("mr_data")
    liver <- m[[get_data_column("broad_stage")]] %in% "Liver stage"
    leaked <- sum(liver & !is.na(m[[get_data_column("pred_gametocyte_sex")]]))
    expect_equal_(leaked, 0L, "liver-stage cells carrying an inferred gametocyte sex")
})

test_that_("non-blood cells have no IDC reference-time class", {
    m <- app_data("mr_data")
    non_blood <- m[[get_data_column("tissue_or_sample_type")]] != "Host blood"
    leaked <- sum(non_blood & !is.na(m[[get_data_column("idc_reference_class")]]))
    expect_equal_(leaked, 0L, "non-blood cells carrying an IDC reference-time class")
})
