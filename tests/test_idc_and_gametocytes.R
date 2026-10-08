# IDC reference-time semantics and gametocyte provenance tests.

# --------------------------------------------------------------------- IDC

test_that_("the obsolete hour_post_invasion field is not required by the app", {
    expect_true_(
        !("hour_post_invasion" %in% required_data_columns()),
        "hour_post_invasion must not be a required app column"
    )
    expect_empty_(
        Filter(function(k) get_data_column(k) == "hour_post_invasion", data_field_keys()),
        "config fields still mapping to hour_post_invasion"
    )
})

test_that_("the IDC reference-similarity field is configured and present", {
    m <- app_data("mr_data")
    col <- get_data_column("idc_reference_class")
    expect_equal_(col, "idc_reference_similarity_label", "configured IDC column")
    expect_true_(col %in% colnames(m), "IDC column missing from the candidate data")
})

test_that_("the IDC field is not labelled as measured hour-post-invasion", {
    label <- get_data_label("idc_reference_class")
    expect_true_(
        !grepl("hour[ _-]*post", label, ignore.case = TRUE) && !grepl("^HPI$", label),
        sprintf("IDC label '%s' still presents itself as measured HPI", label)
    )
    help <- get_data_help("idc_reference_class")
    expect_true_(!is.null(help) && nzchar(help), "IDC field must carry explanatory help text")
    expect_true_(
        grepl("not a measured hour-post-invasion", help, fixed = TRUE),
        "IDC help text must explicitly disclaim measured hour-post-invasion semantics"
    )
    expect_true_(
        grepl("similarity", help, ignore.case = TRUE),
        "IDC help text must describe the field as an expression-similarity classification"
    )
})

test_that_("IDC uncertainty companion fields are retained and configured", {
    m <- app_data("mr_data")
    for (k in c("idc_similarity_raw_label", "idc_similarity_score_delta",
        "idc_similarity_pruned_flag")) {
        expect_true_(
            get_data_column(k) %in% colnames(m),
            sprintf("uncertainty field %s missing from the candidate data", k)
        )
    }
})

test_that_("the heuristic IDC bin is labelled as a PlaViSca heuristic", {
    label <- get_data_label("idc_heuristic_stage_bin")
    expect_true_(
        grepl("heuristic", label, ignore.case = TRUE),
        sprintf("label '%s' must say it is a heuristic", label)
    )
    expect_true_(
        grepl("PlaViSca", label, fixed = TRUE),
        sprintf("label '%s' must attribute the binning to PlaViSca", label)
    )
})

test_that_("no NA IDC values were manufactured", {
    m <- app_data("mr_data")
    col <- get_data_column("idc_reference_class")
    expect_true_(
        sum(is.na(m[[col]])) > 0,
        "cells outside the blood-eligible population must keep a true NA IDC value"
    )
    expect_empty_(
        intersect(unique(as.character(m[[col]])), LITERAL_MISSING_TOKENS),
        "IDC field contains literal missing-ish strings"
    )
})

# -------------------------------------------------------------- gametocytes

test_that_("inferred and author-reported gametocyte sex are distinct fields", {
    pred <- get_data_column("pred_gametocyte_sex")
    src <- get_data_column("source_sex_annotation")
    expect_true_(pred != src, "the two provenance fields must not collapse into one column")
    m <- app_data("mr_data")
    expect_true_(all(c(pred, src) %in% colnames(m)), "both provenance columns must exist")
})

test_that_("the two gametocyte fields are labelled with their provenance", {
    expect_true_(
        grepl("inferred", get_data_label("pred_gametocyte_sex"), ignore.case = TRUE),
        "the inferred field's label must say it is inferred"
    )
    expect_true_(
        grepl("author", get_data_label("source_sex_annotation"), ignore.case = TRUE),
        "the source field's label must say it is author-reported"
    )
})

test_that_("both male and female inferred labels are supported symmetrically", {
    m <- app_data("mr_data")
    vals <- unique(na.omit(as.character(m[[get_data_column("pred_gametocyte_sex")]])))
    expect_true_(
        all(c("Male gametocyte", "Female gametocyte") %in% vals),
        "the candidate must contain both inferred sexes"
    )
    cols <- get_field_colors("pred_gametocyte_sex")
    expect_true_(
        all(c("Male gametocyte", "Female gametocyte") %in% names(cols)),
        "both sexes must have an explicit configured colour"
    )
    expect_true_(
        cols[["Male gametocyte"]] != cols[["Female gametocyte"]],
        "the two sexes must be visually distinguishable"
    )
})

test_that_("adjudicated stage is not a separate app field and mirrors the detailed stage", {
    m <- app_data("mr_data")
    expect_empty_(
        Filter(function(k) get_data_column(k) == "adjudicated_display_stage", data_field_keys()),
        "adjudicated_display_stage duplicates the detailed stage and must not be exposed as a second app field"
    )
    if ("adjudicated_display_stage" %in% colnames(m)) {
        det <- m[[get_data_column("detailed_stage")]]
        adj <- m$adjudicated_display_stage
        expect_true_(
            identical(is.na(adj), is.na(det)) && all(adj[!is.na(adj)] == det[!is.na(det)]),
            "adjudicated_display_stage must equal harmonized_life_cycle_stage (DEC07 resolved)"
        )
    }
})

test_that_("both gametocyte labels reach the detailed stage with configured colours", {
    m <- app_data("mr_data")
    det <- m[[get_data_column("detailed_stage")]]
    for (lab in c("Female gametocyte", "Male gametocyte")) {
        expect_true_(sum(det == lab, na.rm = TRUE) > 0, sprintf("no cell has detailed stage '%s'", lab))
        expect_true_(lab %in% get_field_vocabulary("detailed_stage"), sprintf("'%s' missing from detailed_stage vocabulary", lab))
        expect_true_(lab %in% names(get_field_colors("detailed_stage")), sprintf("'%s' has no configured detailed_stage colour", lab))
        expect_true_(identical(unname(stage_compatibility_map()[[lab]]), "Blood stage"), sprintf("'%s' must map to Blood stage", lab))
    }
})

test_that_("source and inferred sex genuinely disagree for some cells (not collapsed)", {
    m <- app_data("mr_data")
    pred <- m[[get_data_column("pred_gametocyte_sex")]]
    src <- m[[get_data_column("source_sex_annotation")]]
    both <- !is.na(pred) & !is.na(src)
    if (sum(both) == 0) skip_test("no cells carry both annotations in this candidate")
    # The whole point of keeping them separate is that they are not identical.
    expect_true_(
        sum(pred[both] != src[both]) > 0,
        "source and inferred sex are identical everywhere - check they were not merged upstream"
    )
})

# ---------------------------------------------------------------- treatment

test_that_("study_control and No_Treatment remain distinct categories", {
    m <- app_data("mr_data")
    vals <- unique(na.omit(as.character(m[[get_data_column("treatment")]])))
    expect_true_("study_control" %in% vals, "study_control must be present")
    expect_true_("No_Treatment" %in% vals, "No_Treatment must be present")
    labels <- display_values(get_data_column("treatment"), c("study_control", "No_Treatment"))
    expect_equal_(labels, c("Study control", "No treatment"), "human-readable treatment labels")
    expect_true_(labels[1] != labels[2], "the two must not display as the same thing")
})

test_that_("missing treatment is never turned into a treatment category", {
    m <- app_data("mr_data")
    vals <- as.character(m[[get_data_column("treatment")]])
    expect_empty_(
        intersect(unique(vals), LITERAL_MISSING_TOKENS),
        "literal missing-ish strings used as treatment categories"
    )
    # relabel_missing is display-only and must not alter non-missing values
    rl <- relabel_missing(c("study_control", NA, "No_Treatment"))
    expect_equal_(rl, c("study_control", MISSING_VALUE_LABEL, "No_Treatment"),
        "relabel_missing must only touch missing values")
})

# ----------------------------------------------------------------- geography

test_that_("parasite geography uses parasite_origin_location, not the conflated field", {
    expect_equal_(get_data_column("parasite_origin_location"), "parasite_origin_location",
        "primary geography column")
    expect_empty_(
        Filter(function(k) get_data_column(k) == "geographic_location", data_field_keys()),
        "config fields still mapping to the conflated geographic_location"
    )
})

test_that_("the other provenance concepts stay separate fields", {
    m <- app_data("mr_data")
    for (k in c("host_provenance", "experimental_site", "sequencing_site", "registry_location")) {
        expect_true_(
            get_data_column(k) %in% colnames(m),
            sprintf("%s must remain available as a distinct field", k)
        )
    }
    cols <- vapply(c("parasite_origin_location", "host_provenance", "experimental_site",
        "sequencing_site", "registry_location"), get_data_column, character(1))
    expect_equal_(length(unique(cols)), 5L, "the five geography concepts must not be collapsed")
})

# ------------------------------------------------------- biological replicate

test_that_("biological replicate identity is visible in the app", {
    m <- app_data("mr_data")
    for (k in c("animal_id", "donor_id", "biological_replicate_id", "biological_replicate_type")) {
        expect_true_(
            get_data_column(k) %in% colnames(m),
            sprintf("%s missing from the candidate data", k)
        )
        expect_true_(
            k %in% detail_table_field_keys(),
            sprintf("%s must be surfaced in the metadata detail view", k)
        )
    }
})
