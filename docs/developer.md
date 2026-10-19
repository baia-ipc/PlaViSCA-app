# PlaViSCA developer guide

How the app is organised and how to change it. For installing and running
the app, see the [README](../README.md); for using it, see the
[user manual](../manual/docs/source/user_manual.md).

## How the app starts

`app.R` sources three files and starts Shiny:

1. **`global.R`** — loads packages, sources `functions/`, reads the data from
   `PLAVISCA_DATA_DIR` and runs the startup checks
   (`stop_if_app_data_invalid()` in `functions/data/checks.R`). If a check
   fails the app stops here, with a message listing the failed checks.
2. **`ui.R`** — sources `ui/` and assembles the page from one function per
   section (`ui_menu_bar()`, `ui_analysis_section()`).
3. **`server.R`** — the `server()` function. It calls the server code in
   `server/` (one file per tab or shared part, see below) and passes it the
   reactives the parts share: the loaded data and the filter values of each
   tab.

## Where things are

| Path | Content |
|---|---|
| `config/data_columns.yml` | the data columns the app uses (see below) |
| `config/external_links.yml` | every external URL; read with `external_link()` |
| `config/team.yml` | team, organisations, contacts; read with `team_info()` |
| `functions/config/` | reading the configuration: `data_columns.R` (`get_data_column()`, `get_data_label()`, orderings, colours, stage mapping), `site.R` (`external_link()`, `team_info()`), `data_paths.R` (where the data files are, `PLAVISCA_DATA_DIR`) |
| `functions/data/` | working with the data, without Shiny: startup checks (`checks.R`: `check_app_data()`, `stop_if_app_data_invalid()`), filtering cells (`filters.R`), gene lists and name/ID conversion (`genes.R`), per-plot expression tables and the "Data" download of a gene expression card (`gene_expression.R`), per-group summaries of gene expression (`gene_summaries.R`: `gene_group_summary()`), counting cells by one or two fields (`cell_counts.R`), the expressed genes of one cell (`cell_details.R`), the Data sources table (`data_sources.R`), cache keys (`cache_keys.R`) |
| `functions/plots/` | plotly plots: QC (`qc.R`), cell counts of the Metadata plots tab (`cell_counts.R`), the plot types of a gene expression card (`gene_expression.R`: `GENE_PLOT_TYPES`, `GENE_PLOT_OPTIONS`, `gene_expression_card_plot()`), violin and box plots (`gene_distribution.R`), plots of per-group summaries (`gene_summaries.R`), embeddings (`embeddings.R`), their legends (`embedding_legend.R`: position and margins), embeddings coloured by genes (`embedding_gene_colours.R`: "Combine genes", `blend_gene_colours()`, the colour key), the embedding plot of a set of cells (`embedding_figure.R`: `embedding_figure()`) |
| `functions/ui/` | UI building blocks: filter rows, gene links, logos (`helpers.R`), the card of the Gene expression tab (`gene_expression_card.R`), the plot builder of the Metadata plots tab (`plot_builder.R`, a Shiny module: `plot_builder_ui()`, `plot_builder_server()`) |
| `server/` | server code, one file per tab or shared part (see [The server code](#the-server-code)) |
| `ui/` | the top bar (`page_frame.R`), the tab set (`analysis.R`) and one file per tab (`tab_*.R`, `about.R`) |
| `www/` | `style.css`, `script.js` and images served to the browser |
| `tests/` | test suite, see [Tests](#tests) |
| `tools/screenshots/` | screenshots of the app in a headless browser, for the manual and for figures ([README](../tools/screenshots/README.md)) |

Rules the tests enforce:

- data column names appear only in `config/data_columns.yml`; code uses
  `get_data_column("broad_stage")`, never `"broad_stage"` directly;
- data file paths are built only in `functions/config/data_paths.R`;
- external URLs appear only in `config/external_links.yml` and
  `config/team.yml`;
- no code uses the column names listed under `unsupported_columns` in
  `config/data_columns.yml`.

## The server code

`server.R` connects the files of `server/`. Each file defines a function
`<part>_server(input, output, ...)`: the reactives it needs come in as
arguments, the ones other parts need are returned.

| File | Content | Returns |
|---|---|---|
| `data_loading.R` | the expression data of the session, raw or normalized; keeps the two Data type controls in sync | `gene_data`, `data_type`, `cached_data`, `global_loading` |
| `filter_choices.R` | the choices of the filter menus; the coupling of the broad and detailed stage filters | the choices of each filter |
| `shared_filters.R` | the filter state shared by the tabs | the filter values of each tab |
| `tab_embeddings.R` | Embeddings tab: the filtered cells, the single plot, Download Data | |
| `embedding_controls.R` | axes, point size, gene menus, Combine genes menus and gene links of the embedding plots | |
| `embedding_compare.R` | Compare Plots: the two plots side by side | |
| `cell_details.R` | the window with the genes of a cell opened in an embedding plot | |
| `tab_gene_expression.R` | Gene expression tab: the filtered cells, Group by, adding and removing cards, the name/ID switch | the IDs of the cards |
| `gene_card.R` | one card of the Gene expression tab: plot, gene links, Data download | |
| `tab_plots.R` | Metadata plots tab | |
| `tab_data_table.R`, `tab_qc.R`, `tab_data_sources.R` | Metadata table, QC Plots and Data sources tabs | |

To add a tab: build its page in `ui/tab_<name>.R`, write
`<name>_tab_server()` in `server/tab_<name>.R`, source it in `global.R`,
call it from `server.R`, and add its checks to `tests/server/tab_<name>.R`.
If it has filters, add it to `FILTER_TAB_SUFFIXES` in
`server/shared_filters.R` and pass it `tab_filters[["<tab title>"]]`.

## The data column configuration

`config/data_columns.yml` maps a *field key* used by the code to a column of
the data, with its label and help text:

```yaml
fields:
  broad_stage:
    label: "Broad life-cycle stage"   # shown in the UI
    column: broad_stage               # column in the data files
    role: filter
    required: true                    # checked at startup
    help: >
      Blood, liver or sporozoite stage.
```

Missing values are shown as the global `missing_label` ("Missing"), listed
last and coloured grey. A field can name them itself with its own
`missing_label:`; for example, `gametocyte_state` shows "No gametocyte".

Further blocks say where each field is used (`filter_fields`,
`category_fields` for "Color by"/"Group by", `detail_table_fields`,
`plot_fields` for the menus of the Metadata plots tab), and
give biological orderings (`vocabularies`), fixed colours (`colors`),
display labels for coded values (`value_labels`), the detailed → broad stage
mapping (`stage_compatibility`) and the expected studies.

### Tutorial: show a new metadata column

Say the pipeline adds a column `sample_batch` and you want it as a filter and
in the cell table.

1. Declare the field in `config/data_columns.yml`:

   ```yaml
   fields:
     sample_batch:
       label: "Sample batch"
       column: sample_batch
       role: filter
       required: true
       help: >
         Library preparation batch, as given by the authors.
   ```

2. Add `sample_batch` to `filter_fields` (filter row on every tab) and to
   `detail_table_fields` (Metadata table tab). Add it to `category_fields` too if
   it should be offered under "Color by", and to `plot_fields` to count cells
   by it in the Metadata plots tab.
3. If its values have a natural order, list it under `vocabularies`.
4. Run the tests (below). `test_config_fields.R` fails if the column is
   missing from the data; the startup checks would stop the app in the same
   case.

No R code changes are needed: the filter widget, its choices and the table
column are built from the configuration.

### Tutorial: update the app to new data

1. Run the preprocessing pipeline and point `PLAVISCA_DATA_DIR` at its
   `results/app_data/`.
2. Start the app or run the tests. If the studies or columns changed, the
   startup checks say which.
3. Update `expected_studies` and any changed fields in
   `config/data_columns.yml`.

## Tests

```bash
PLAVISCA_DATA_DIR=/path/to/app_data Rscript tests/run_tests.R
```

`tests/helper.R` provides a small dependency-free harness (`test_that_()`,
`expect_equal_()`, ...). Test files, all run by `tests/run_tests.R`:

| File | Checks |
|---|---|
| `test_data_checks.R` | the data load and pass every startup check |
| `test_config_fields.R` | every configured field exists in the data |
| `test_lifecycle.R` | broad/detailed stage mapping, ordering, stage filters |
| `test_idc_and_gametocytes.R` | reference time point, gametocyte, treatment and replicate fields |
| `test_bibliography_and_filters.R` | `data_source.csv` agrees with the data; filter behaviour |
| `test_expression.R` | gene columns, expression lookup, alignment of the tables, name/ID switch of the gene menus |
| `test_cell_count_plots.R`, `test_gene_plots.R` | plots of the Metadata plots and Gene expression tabs |
| `test_embedding_gene_colours.R` | embeddings coloured by genes: mean, sum, scaling, colour mixing, colour keys |
| `test_app_code.R` | the code rules listed above |
| `test_server.R` | the server code of `server/`, with `shiny::testServer()`: runs `tests/server_checks.R` in a separate R process, which loads `global.R` and the checks in `tests/server/` (one file per server file) |

To add a test, add a `test_that_("what it checks", { ... })` block to the
file of that topic, or a new `tests/test_<topic>.R` listed in
`tests/run_tests.R`. A check of server code is a `check("what it checks",
{ ... })` block in `tests/server/`, for example:

```r
check("qc_tab_server counts the cells of the filtered study", {
  testServer(function(input, output, session) {
    qc_tab_server(input, output, reactive(study_filter("Sa2020")))
  }, {
    session$flushReact()
    stop_unless(!is.null(output$data_count_plotly), "no RNA count plot")
  })
})
```

## The user manual

The manual is a [Sphinx](https://www.sphinx-doc.org/) project with Markdown
pages in `manual/docs/source/` (`user_manual.md`, `setup_guide.md`). The app
serves the HTML build in `manual/docs/build/html/` (the *Manual* tab), so
rebuild and commit it after editing the pages:

```bash
pip install sphinx myst-parser sphinx-rtd-theme
sphinx-build -E -b html manual/docs/source manual/docs/build/html
```

The images in `manual/docs/source/imgs/` are screenshots of the app, made with
`tools/screenshots/` from the shot list `tools/screenshots/manual.yml`. Retake
them after a change of the interface (see its
[README](../tools/screenshots/README.md)).

## Code style

- Follow the style of the file you edit: R files in `functions/` use 4-space
  indentation, `ui/`, `server/`, `ui.R` and `server.R` use 2 spaces.
- `<-` for assignment, `snake_case` for functions, variables and Shiny IDs,
  `UPPER_SNAKE_CASE` for constants (`DATA_PATHS`).
- Keep files short and about one topic (at most a few hundred lines); put
  logic that does not need Shiny in `functions/` so it can be tested.
- In reactive code: `req()` before using an input; `observeEvent()` for side
  effects; `reactive()`/`eventReactive()` for values; `debounce()` for inputs
  that trigger heavy plots.
- The expression tables are large: filter by row indices instead of copying
  them, and wrap slow plots in `withSpinner()`.
- A mismatch between data and configuration is an error with a clear
  message, never a silent fallback to empty or unfiltered data.
- Check that a package is in `renv.lock` before adding a `library()` call;
  record new packages with `renv::snapshot()`.
