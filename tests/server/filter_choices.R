check("filter_choices_server takes the filter choices from the data", {
  testServer(function(input, output, session) {
    choices <- filter_choices_server(input, session, reactive(list(data = mr_data)))
  }, {
    session$flushReact()
    studies <- unlist(choices[["study_label"]])
    stop_unless(setequal(studies, unique(mr_data$study_label)), "study choices are not the studies of the data")
  })
})

check("stage_filters_server reports an impossible stage combination", {
  testServer(function(input, output, session) {
    stage_filters_server(input, output, session)
  }, {
    session$setInputs(broad_stage_qc = "Liver stage", detailed_stage_qc = "Trophozoite")
    stop_unless(grepl("No cells can match", as.character(output$stage_compatibility_message_qc$html)),
                "no message for Liver stage + Trophozoite")
    session$setInputs(broad_stage_qc = "Blood stage")
    stop_unless(is.null(output$stage_compatibility_message_qc),
                "message for a possible combination")
  })
})
