library(shiny)
library(shinyjs)
library(plotly)

ui <- fluidPage(
  useShinyjs(),

  # Link to CSS and JS files
  tags$link(rel = "stylesheet", type = "text/css",
            href = "style.css"),
  tags$script(src = "script.js"),

  # Menu Bar
  div(
    class = "menu-bar",
    div(class = "logo", img(src = "Pvivax.jpeg", height = "60px")),
    div(
      class = "menu-title",
      span("Plasmodium vivax")
    ),
    div(
      class = "menu-items",
      actionLink("home",
        label = tagList(icon("home"), " Home"),
        class = "menu-link"
      ),
      span("|"),
      actionLink("data",
        label = tagList(icon("table"), " Data display"),
        class = "menu-link"
      )
    )
  ),

  # Home Section
  div(
    id = "home_section",
    fluidRow(
      class = "introduction",
      p("Malaria caused by Plasmodium vivax remains a significant
                 public health challenge in many parts of the world. While
                 less lethal than Plasmodium falciparum, P. vivax infections
                 are more widespread and persistent, causing recurrent relapses
                 that complicate elimination efforts. Its ability to remain
                 dormant in the liver for prolonged periods and resurface
                 months or even years later makes it uniquely difficult to treat
                 and eradicate. Despite its global burden, our understanding of
                 P. vivax biology, epidemiology, and treatment lags behind,
                 partly due to limited data sharing and collaboration across
                 research communities."),
      p("One of the critical barriers in combating P. vivax malaria is
                 the fragmented nature of data collection and knowledge sharing.
                 Key datasets on genetic diversity, drug resistance, and disease
                 patterns are often siloed within institutions, limiting
                 comprehensive analyses and insights. This lack of coordination
                 hampers progress in developing robust strategies to tackle
                 P. vivax infections. Furthermore, regions most affected by
                 P. vivax, such as South Asia, Southeast Asia, and South America,
                 often have limited resources to gather and analyze the data
                 necessary for actionable interventions. Bridging these gaps
                 requires a collective effort to standardize, centralize, and
                 share knowledge."),
      p("To address these challenges, we have developed a platform
                 dedicated to facilitating the sharing of Plasmodium
                 vivax-related data and knowledge. This innovative solution
                 seeks to bridge gaps in global collaboration by integrating
                 diverse datasets, promoting transparency, and offering advanced
                 tools for data analysis and visualization. By empowering
                 researchers and policymakers with centralized access to
                 critical information, the platform fosters a culture of open
                 data sharing. Our goal is to accelerate scientific discoveries,
                 refine treatment protocols, and contribute meaningfully to the
                 global fight against P. vivax malaria, paving the way for its
                 eventual elimination.",
        actionLink("explore_data", "Explore data >>")
      ),
    ),
    div(class = "img", img(src = "pvivax.jpg"))
  ),

  # Data Section
  div(
    id = "data_sets_section",
    fluidRow(
      column(
        width = 4, id = "sidebar", class = "sidebar",
        checkboxGroupInput(
          "location", "INFECTION STAGE:",
          choices = list(
            "Human Blood" = "human_blood",
            "Human Liver" = "human_liver",
            "Mosquito Saliva" = "mosquito_saliva"
          ),
          selected = list("human_blood", "human_liver", "mosquito_saliva")
        ),
        checkboxGroupInput(
          "stagelr", "Parasite stage:",
          choices = list(
            "Trophozoite" = "trophozoite",
            "Ring" = "ring",
            "Schizont" = "schizont",
            "Gametocyte" = "gametocyte",
            "Hypnozoite" = "hypnozoite",
            "Hepatic schizont" = "hepatic_schizont",
            "zygot" = "zygot",
            "Ookinete" = "ookinete",
            "Oocyst" = "oocyst",
            "Sporozoite" = "sporozoite"
          ),
          selected = list(
            "trophozoite", "ring", "schizont", "gametocyte",
            "hypnozoite", "hepatic_schizont", "zygot", "ookinete",
            "oocyst", "sporozoite"
          )
        ),
        radioButtons("size", "Size:",
          choices = list("S" = 2, "M" = 4, "L" = 6),
          selected = 4
        ),
        radioButtons("color", "Color:",
          choices = list(
            "Stage" = "stage",
            "Strain" = "vivax_strain",
            "Day" = "day",
            "Condition" = "condition",
            "Replicate" = "replicate",
            
            "CCA resolution" = "cca_res"
          ),
          selected = "stage"
        ),
        uiOutput("cca_res_ui"),
        radioButtons("plot", "plot",
          choices = list("2D" = "2d", "3D" = "3d"),
          selected = "2d"
        ),
        fluidRow(
          column(width = 6, uiOutput("xaxis")),
          column(width = 6, uiOutput("yaxis"))
        ),
        downloadButton("download_data", "Download", class = "bt-download")
      ),
      column(width = 8, tabsetPanel(
        id = "tabset",
        tabPanel("DATA",
                 DT::DTOutput("data_table"),
                 plotlyOutput("data_count_plotly"),
                 plotlyOutput("data_feature_plotly")
                 ),
        tabPanel("PCA", plotlyOutput("pca_plotly")),
        tabPanel("UMAP", plotlyOutput("umap_plotly")),
        tabPanel("tSNE", plotlyOutput("tsne_plotly"))
      ))
    )
  )
)
