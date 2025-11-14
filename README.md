# PlaViSCA — Plasmodium vivax Single-Cell Analysis (Interactive Shiny App)

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-%3E%3D%201.7.0-brightgreen?logo=r&logoColor=white)](https://shiny.rstudio.com/)
[![Version](https://img.shields.io/badge/version-0.1.0-orange.svg)](https://github.com/Sopheap15/PlaViSca/releases)

PlaViSCA is an interactive R/Shiny application for exploring single-cell RNA-seq
data from Plasmodium vivax. The app is intended for researchers, public health
professionals, and educators who want to visualize embeddings, explore gene
expression across stages, and filter datasets without writing code.

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

3. Run the app

From RStudio: open `app.R` and click "Run App".

From a terminal (GUI):

```bash
R -e "shiny::runApp('.', launch.browser = TRUE)"
```

From a server (no GUI):

```bash
R -e "shiny::runApp('.', host='0.0.0.0', port=3838)"
```

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

## Features and navigation

- Home: high-level overview and links to the manual and data sources.
- Explore Data: filter datasets, change color/symbol mapping, switch
  between integrated/unintegrated embeddings, and toggle between 2D/3D.
- Gene Expression: view per-gene expression across groups and download
  results.

## Contributing

Contributions are welcome! Please:

1. Open an issue to discuss major changes or bugs.
2. Fork the repository and create a feature branch.
3. Submit a pull request with a clear description and, where applicable,
   reproducible examples.

## License & attribution

This project is provided under the CC-BY 4.0 license (see `LICENSE`).

## Acknowledgements

PlaViSCA was developed by the BAIA Team with contributions from the
Plasmodium research community.

## Contact

For questions or support, open an issue on GitHub or contact the maintainers
via the repository.

---

© 2025 BAIA Team. All rights reserved.
