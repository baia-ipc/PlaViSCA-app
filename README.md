# PlaViSCA — _Plasmodium vivax_  Single-Cell RNA-seq Meta-Analysis and Interactive Atlas

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-%3E%3D%201.7.0-brightgreen?logo=r&logoColor=white)](https://shiny.rstudio.com/)
[![Version](https://img.shields.io/badge/version-0.1.0-orange.svg)](https://github.com/Sopheap15/PlaViSca/releases)

PlaViSCA is an interactive R/Shiny application for exploring single-cell RNA-seq
data from _Plasmodium vivax_.

## Key features

- Interactive 2D and 3D visualizations (Plotly-powered PCA / UMAP / t-SNE).
- Per-cell metadata exploration (filter, subset, and download results).
- Gene expression viewer with direct links to PlasmoDB and NCBI.
- Export plots and metadata for downstream analysis.

## Repository layout

After cloning, the repository contains:

```
.
├── app.R                   # Shiny app entry point (loads ui.R and server.R)
├── global.R                # Shared setup (data loading, helpers)
├── ui.R                    # User interface
├── server.R                # Shiny server logic
├── functions/              # Plotting/helpers and preprocessing scripts
├── data/                   # Example and processed RDS files (not all data included)
├── ref/                    # Reference data (e.g gene annotation)
├── www/                    # Static assets (images, CSS, JS)
├── manual/                 # Manual to setup and run the application
└── README.md               # This file
```

## Getting started

1. Clone the repository

```bash
git clone https://github.com/Sopheap15/PlaViSca.git
cd PlaViSca
```

2. Install R dependencies

```r
install.packages('renv')
renv::restore()
```

Manual (if you don't use renv):

```r
install.packages(c(
  'shiny', 'shinyjs', 'shinycssloader', 'plotly', 'DT',
  'RColorBrewer', 'tidyverse', 'later'
))
```

### 3. Run the app

```bash
chmod +x run_app.sh
./run_app.sh
```
Run `./run_app.sh --help` for options (host, port, etc.).

Then open `http://<server-ip>:3838` in a browser.

## Data

The data files are made by the preprocessing pipeline
([PlaViSca_pre_processing](https://github.com/baia-ipc/PlaViSca_pre_processing),
stage `50_export_app`, folder `results/app_data/` of a run):

- `cleaned_dataset.rds`: per-cell metadata and embeddings (`mr_data`), and
  per-study PCA and variable-gene tables;
- `normalize_df.rds`, `raw_df.rds`: metadata plus log-normalised expression
  or counts;
- `data_source.csv`: one row per study;
- `gene_annotation.rds`: gene IDs, names and descriptions.

The per-cell columns are described in the pipeline's `metadata/columns.tsv`.

### Choosing a data directory

The app reads its data from the folder named by the `PLAVISCA_DATA_DIR`
environment variable (default `data`):

```bash
PLAVISCA_DATA_DIR=/path/to/app_data R -e "shiny::runApp('.')"
```

### Data contract

At startup the app validates the loaded dataset against
`config/data_columns.yml` (cell count, expected studies, every configured
field, cell-id uniqueness, broad/detailed life-cycle stage consistency,
expression/metadata alignment, bibliography coverage, and more). If the data
does not match, the app **stops with an explicit message** rather than
starting in a degraded state. `config/data_columns.yml` is the single source
of truth for metadata field names, labels, help text, biological ordering and
colours - never hardcode a metadata column name elsewhere in the app.

## Tests

```bash
PLAVISCA_DATA_DIR=/path/to/export Rscript tests/run_tests.R
```

Plain R scripts, no test framework required. Exit status is non-zero if any
test fails. Optionally pass an output path to write a TSV of results.

## Features and navigation

- Home: high-level overview and links to the manual and data sources.
- Explore Data: filter datasets, change color/symbol mapping, switch
  between integrated/unintegrated embeddings, and toggle between 2D/3D.
- Gene Expression: view per-gene expression across groups and download
  results.

## License & attribution

This project is provided under the CC-BY 4.0 license (see `LICENSE`).

