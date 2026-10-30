# About tab: what PlaViSCA contains, how the data were processed, citation
# and contact.

ui_tab_about <- function() {
  contacts <- team_info()$contacts
  citation <- team_info()$citation
  n_cells <- format(nrow(mr_data), big.mark = ",")
  n_genes <- format(nrow(expression_data("normalize")$genes), big.mark = ",")
  n_studies <- length(unique(mr_data[[get_data_column("study_label")]]))

  tabPanel(
    tagList(fontawesome::fa("circle-info"), " About"),
    value = "About",
    div(
      class = "about-content",
      h3("About PlaViSCA"),
      p(
        "PlaViSCA is an atlas of ", em("Plasmodium vivax"), " single-cell ",
        "transcriptomes. It brings together ", n_cells, " cells and ", n_genes,
        " genes from ", n_studies, " published single-cell RNA-seq studies, ",
        "covering sporozoites, liver stages (including hypnozoites) and blood ",
        "stages (including gametocytes)."
      ),
      p(
        "All studies were processed with one pipeline: the reads were aligned ",
        "to the ", em("P. vivax"), " P01 genome with the PlasmoDB-68 gene models ",
        "(the authors' count matrices were used where the reads could not be ",
        "reprocessed), the metadata were harmonized, the studies were ",
        "integrated with Harmony, and the life-cycle stage of each cell was ",
        "annotated. The app shows the integrated embeddings, the expression of ",
        "selected genes, the metadata of the cells and quality-control plots; ",
        "all views share the same filters."
      ),
      p(
        "The code of the ",
        a("app", href = external_link("source_code"), target = "_blank"),
        " and of the ",
        a("analysis pipeline", href = external_link("analysis_source_code"), target = "_blank"),
        " is open source."
      ),
      h3("Citation"),
      # one HTML string, so that no space is added before the full stops; the
      # title may contain <em>
      p(HTML(sprintf(
        "<em>%s</em>. <strong>%s</strong>. %s",
        htmltools::htmlEscape(citation$authors), citation$title,
        htmltools::htmlEscape(citation$status)
      ))),
      h3("Contact"),
      # one HTML string per contact, so that no space is added around the
      # name and link; the topics may contain <em>
      lapply(contacts, function(contact) {
        p(HTML(sprintf(
          '<strong>%s</strong>, %s, %s (<a href="mailto:%s">%s</a>), for questions related to %s.',
          htmltools::htmlEscape(contact$name), htmltools::htmlEscape(contact$role),
          htmltools::htmlEscape(contact$affiliation), htmltools::htmlEscape(contact$email),
          htmltools::htmlEscape(contact$email), contact$topics
        )))
      })
    )
  )
}
