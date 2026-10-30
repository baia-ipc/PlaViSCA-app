# PlaViSCA — _Plasmodium vivax_ single-cell atlas

[![R](https://img.shields.io/badge/R-4.3-blue?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-app-brightgreen?logo=r&logoColor=white)](https://shiny.posit.co/)

PlaViSCA is an interactive R/Shiny application for exploring single-cell
RNA-seq data of _Plasmodium vivax_. It brings together about 106,000 cells from
six published studies (blood stages, liver stages and sporozoites) and lets
you, without writing code:

- explore the cells on PCA, UMAP and t-SNE plots (2D or 3D), coloured by
  study, life-cycle stage, host, treatment or gene expression, and compare
  two colourings side by side;
- plot the expression of one or more genes per group (violin and box plots,
  Cleveland and Seurat-style dot plots, heatmaps), with links to PlasmoDB and
  NCBI;
- count the cells by one or two metadata fields (e.g. the life-cycle stages
  of each study) as bar, pie, heatmap or sunburst plots;
- filter all views by study, life-cycle stage, host, tissue, treatment,
  strain and reference time point;
- look at quality-control plots, the per-cell metadata and the source
  publications, and download data and plots.

## Installation

You need R 4.3 and git. The package versions are fixed with
[renv](https://rstudio.github.io/renv/), which installs them for you:

```bash
git clone https://github.com/baia-ipc/PlaViSCA-app.git
cd PlaViSCA-app
R -e 'renv::restore()'
```

The `.Rprofile` in this directory activates renv whenever R starts here.

## Quick start

The app reads its data from the directory named by the `PLAVISCA_DATA_DIR`
environment variable (default: `data/` inside the app directory). The data
files are written by the
[PlaViSCA preprocessing pipeline](https://github.com/baia-ipc/PlaViSCA-meta-analysis)
into the `results/app_data/` directory of a pipeline run.

```bash
PLAVISCA_DATA_DIR=/path/to/run/results/app_data ./run_app.sh
```

Then open <http://localhost:3838> in a browser. `./run_app.sh --help` lists
the options (host, port, opening a browser). From R or RStudio, the same is:

```r
Sys.setenv(PLAVISCA_DATA_DIR = "/path/to/run/results/app_data")
shiny::runApp(".", port = 3838)
```

## Data

| File | Content |
|---|---|
| `cleaned_dataset.rds` | per-cell metadata and embeddings (`mr_data`), per-study PCA and variable-gene tables |
| `normalized_expression.rds`, `raw_expression.rds` | log-normalised expression or raw counts: a sparse matrix (`Matrix::dgCMatrix`), cells x genes, with the cell barcodes as row names (in the order of `mr_data`) and the gene IDs as column names |
| `gene_annotation.rds` | gene identifiers, names and descriptions |
| `data_source.csv` | one row per study: authors, title, journal, date, PMID, DOI, cell count |

The per-cell columns are described in the pipeline's `metadata/columns.tsv`.

### Startup checks

At startup the app checks that the data match what it expects, as set in
`config/data_columns.yml`: the number of cells and the list of studies, that
every configured column is present, that cell identifiers are unique, that
broad and detailed life-cycle stages agree, that the rows of the expression
matrices are the cells of the metadata in the same order, and that `data_source.csv` lists
exactly the studies in the data. If a check fails, the app stops with a
message saying what is wrong, for example:

```
PlaViSCA cannot start: 1 data check(s) failed.
The data in '/path/to/app_data' do not match config/data_columns.yml:
  - cell_count: expected 105949 metadata rows, found 98000
```

When the data change on purpose (for example a new study), update
`config/data_columns.yml` together with them.

## Configuration

| File | What it sets |
|---|---|
| `config/data_columns.yml` | which data columns the app uses, their labels, help text, ordering and colours, and the expected studies |
| `config/external_links.yml` | every external web address (PlasmoDB and NCBI gene pages, source code, front-end libraries) |
| `config/team.yml` | team, organisations and contacts shown on the About page and in the menu bar |

## Tests

```bash
PLAVISCA_DATA_DIR=/path/to/run/results/app_data Rscript tests/run_tests.R
```

The tests are plain R scripts (no test framework needed). They check the
data, the configuration and the app code; the exit status is non-zero if a
test fails. Pass a file name to also write the results as a table:
`Rscript tests/run_tests.R results.tsv`.

## Documentation

- **User manual** — how to use each page of the app:
  [manual/docs/source/user_manual.md](manual/docs/source/user_manual.md),
  and the setup guide [manual/docs/source/setup_guide.md](manual/docs/source/setup_guide.md).
  In the app, the *User manual* button opens the HTML version.
- **Developer guide** — how the code is organised, how to add a data field,
  run the tests and rebuild the manual: [docs/developer.md](docs/developer.md).

## Repository layout

```
app.R, global.R, ui.R, server.R   Shiny entry point, startup, page, server logic
run_app.sh                        start the app from the command line
config/                           data columns, external links, team
functions/                        helpers by topic: config/, data/, plots/, ui/
ui/                               one file per page section and analysis tab
server/                           server code, one file per tab or shared part
www/                              images, CSS and JavaScript served to the browser
tests/                            test suite (tests/run_tests.R)
manual/                           user manual (Sphinx sources and HTML build)
docs/                             developer documentation
renv.lock, renv/                  R package versions
PlaViSCA-app.Rproj                RStudio project
```

## License

PlaViSCA is released under the Creative Commons Attribution 4.0
International license (CC BY 4.0); see [LICENSE](LICENSE).
