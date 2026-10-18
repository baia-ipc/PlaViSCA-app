library(shiny)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(bslib)
library(fontawesome)

# Page parts, one file per section or tab.
source("ui/page_frame.R")
source("ui/tab_embeddings.R")
source("ui/tab_gene_expression.R")
source("ui/tab_plots.R")
source("ui/tab_data_table.R")
source("ui/tab_qc.R")
source("ui/tab_data_sources.R")
source("ui/tab_manual.R")
source("ui/analysis.R")
source("ui/about.R")

ui <- fluidPage(
  theme = bs_theme(version = 5),
  ui_head(),
  ui_menu_bar(),
  ui_analysis_section()
)
