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

The app expects preprocessed data stored under `data/`. The repository includes
examples and processed RDS snapshots used for development. The preprocessing
pipeline is available in `functions/preprocessing.R` — it flattens a Seurat
object and produces the RDS files that the app reads.

Typical files in `data/` (examples):

- `cleaned_dataset.rds`
- `data_source.csv`
- `normalize_df.rds`
- `raw_df.rds`
- `PvivaxP01_gff_data.rds` (reference GFF)

If you use your own data, ensure it follows the same structure (metadata
columns, embedding column names like `umap_i_1`, `umap_i_2`, and gene
expression columns). See `functions/build_plot_slim()` for details on expected
columns.

### Choosing a data directory

All application data paths are resolved from the `PLAVISCA_DATA_DIR`
environment variable, which defaults to `data`. To run the app against a
different export (for example a release candidate produced by the
`pre_process_data` pipeline) without copying anything:

```bash
PLAVISCA_DATA_DIR=/path/to/export R -e "shiny::runApp('.')"
```

The directory must contain `cleaned_dataset.rds`, `normalize_df.rds`,
`raw_df.rds` and `data_source.csv`.

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

