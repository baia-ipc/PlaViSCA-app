# Home section: welcome text and links to the analysis tabs.

ui_home_section <- function() {
  div(
    id = "home_section",
    class = "home-section",
    div(id = "particles-js-home"),
    fluidRow(
      class = "introduction",
      column(
        width = 6,
        class = "intro-left",
        h2("Welcome to PlaViSca"),
        p(
          "An interactive platform for the analysis and visualization of single-cell RNA sequencing (scRNA-seq) data from ",
          em("P. vivax"),
          ". Explore metadata, gene expression, and various parasite life cycle stages through interactive tables and 2D/3D visualizations."
        )
      ),
      column(
        width = 6,
        class = "intro-right",
        div(
          class = "pages-links",
          actionLink(
            "metadata_link",
            tagList(
              img(src = "metadata.png", alt = "Metadata", class = "link-icon"),
              span("Metadata")
            )
          ),
          actionLink(
            "analysis_link",
            tagList(
              img(src = "umap.png", alt = "Embedding", class = "link-icon"),
              span("Embedding")
            )
          ),
          actionLink(
            "qc_link",
            tagList(
              img(src = "qc.png", alt = "Quality control", class = "link-icon"),
              span("Quality control")
            )
          ),
          actionLink(
            "sources_link",
            tagList(
              img(src = "data_sources.png", alt = "Data sources", class = "link-icon"),
              span("Data sources")
            )
          ),
          actionLink(
            "gene_exp_link",
            tagList(
              img(src = "gene_exp.png", alt = "Gene Expression", class = "link-icon"),
              span("Gene expression")
            )
          ),
          actionLink(
            "usermanual_link",
            tagList(
              img(src = "usermanual.png", alt = "User Manual", class = "link-icon"),
              span("User Manual")
            )
          )
        )
      )
    )
  )
}
