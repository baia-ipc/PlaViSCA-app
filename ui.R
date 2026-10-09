library(shiny)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(bslib)
library(fontawesome)

# Page parts, one file per section or tab.
source("ui/page_frame.R")
source("ui/home.R")
source("ui/tab_embeddings.R")
source("ui/tab_gene_expression.R")
source("ui/tab_qc.R")
source("ui/tab_metadata.R")
source("ui/tab_data_sources.R")
source("ui/analysis.R")
source("ui/about.R")

ui <- fluidPage(
  theme = bs_theme(version = 5),
  ui_head(),
  ui_menu_bar(),
  ui_home_section(),
  ui_analysis_section(),
  ui_about_section(),
  ui_footer()
)
