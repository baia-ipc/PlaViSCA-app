# About section: introduction, capabilities, team (config/team.yml) and
# collaboration.

# About section.
ui_about_section <- function() {
  div(
    id = "about_section",
    style = "display: none;",
    class = "about-section",
    div(
      class = "about-container",
      # Hero Section
      div(
        class = "about-page-wrapper",
        div(id = "particles-js-about", class = "particles-js-full"),
        div(
          class = "about-content-minimal",
          # Introduction
          div(
            class = "text-center mb-5",
            h1("What is PlaViSca?", class = "minimal-title"),
            p(class = "minimal-subtitle", tags$em("Plasmodium vivax"), " Single-Cell Analysis Platform")
          ),
          div(
            class = "about-section-condensed mb-5",
            p(
              strong("PlaViSca"), " is a high-performance platform for the multi-dimensional exploration of ", em("P. vivax"), " single-cell transcriptomics. We provide researchers with the tools to translate complex genomic data into impactful biological insights."
            )
          ),

          # Capabilities
          div(
            class = "about-section-condensed mb-5",
            h3(class = "minimal-section-header", "Key Capabilities"),
            tags$ul(
              class = "minimal-list",
              tags$li(strong("professional embeddings:"), " Interactive navigation of UMAP, PCA, and t-SNE projections."),
              tags$li(strong("gene expression:"), " High-resolution visualization of expression across life stages."),
              tags$li(strong("unified metadata:"), " Centralized access to curated multi-study datasets."),
              tags$li(strong("comparative analytics:"), " Advanced side-by-side analysis of biological states.")
            )
          ),

          # Team Section - Professional Cards
          div(
            class = "about-section-condensed mb-5",
            h3(class = "minimal-section-header", "Development Team"),
            create_team_cards()
          ),

          # Collaboration & Open Science Section
          div(
            class = "collaboration-box",
            h3("Collaboration & Open Science", class = "collab-header"),
            div(
              class = "collab-grid",
              div(
                class = "collab-item",
                fontawesome::fa("microscope"),
                h4("open source mission"),
                p("open science is at our core. we believe in transparent, reproducible research and provide open source tools to accelerate biological discovery globally.")
              ),
              div(
                class = "collab-item",
                fontawesome::fa("database"),
                h4("dataset integrations"),
                p("we welcome raw and processed dataset submissions to expand our multi-dimensional view of ", tags$em("P. vivax"), " single-cell biology.")
              ),
              div(
                class = "collab-item",
                fontawesome::fa("globe"),
                h4("global partnerships"),
                p("PlaViSca thrives on international collaboration. we invite researchers and institutions to join our network to extend the platform and seek funding to support these activities.")
              )
            ),
            div(
              class = "github-footer-minimal",
              a(
                href = external_link("source_code"),
                target = "_blank",
                class = "github-link-beautiful",
                fontawesome::fa("github"), " Explore Source Code on GitHub"
              )
            )
          )
        )
      )
    )
  )
}
