# PlaViSca developer guide

How the app is organised and how to change it. For installing and running
the app, see the [README](../README.md); for using it, see the
[user manual](../manual/docs/source/user_manual.md).

## How the app starts

`app.R` sources three files and starts Shiny:

1. **`global.R`** — loads packages, sources `functions/`, reads the data from
   `PLAVISCA_DATA_DIR` and runs the startup checks
   (`stop_if_app_data_invalid()` in `functions/data_checks.R`). If a check
   fails the app stops here, with a message listing the failed checks.
2. **`ui.R`** — sources `ui/` and assembles the page from one function per
   section (`ui_home_section()`, `ui_analysis_section()`, ...).
3. **`server.R`** — all server logic in one `server()` function, organised in
   commented sections: navigation, data loading and caching, filters, one
   section per analysis tab, embedding plots and comparison mode, cell
   details, downloads.

## Where things are

| Path | Content |
|---|---|
| `config/data_columns.yml` | the data columns the app uses (see below) |
| `config/external_links.yml` | every external URL; read with `external_link()` |
| `config/team.yml` | team, organisations, contacts; read with `team_info()` |
| `functions/data_paths.R` | where the data files are (`PLAVISCA_DATA_DIR`) |
| `functions/data_checks.R` | startup checks: `check_app_data()`, `stop_if_app_data_invalid()` |
| `functions/data_columns_config.R` | accessors for `data_columns.yml`: `get_data_column()`, `get_data_label()`, orderings, colours, stage mapping |
| `functions/site_config.R` | accessors for `external_links.yml` and `team.yml` |
| `functions/filters.R` | reading the filter widgets and filtering cells |
| `functions/genes.R`, `gene_expression_data.R` | gene lists, name/ID conversion, per-plot expression tables |
| `functions/plots_*.R`, `embedding_plots.R` | plotly plots of each tab |
| `functions/data_summary.R`, `tab_sync.R` | selection summaries; keeping tabs in sync, cache keys |
| `functions/helpers_UI.R` | UI building blocks: filter rows, gene links, team cards |
| `ui/` | one file per page section (`home.R`, `about.R`, `page_frame.R`) and analysis tab (`tab_*.R`) |
| `www/` | `style.css`, `script.js` and images served to the browser |
| `tests/` | test suite, see [Tests](#tests) |

Rules the tests enforce:

- data column names appear only in `config/data_columns.yml`; code uses
  `get_data_column("broad_stage")`, never `"broad_stage"` directly;
- data file paths are built only in `functions/data_paths.R`;
- external URLs appear only in `config/external_links.yml` and
  `config/team.yml`;
- no code uses the column names listed under `unsupported_columns` in
  `config/data_columns.yml`.

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
`category_fields` for "Color by"/"Group by", `detail_table_fields`), and
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
   `detail_table_fields` (Data table tab). Add it to `category_fields` too if
   it should be offered under "Color by".
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
| `test_expression.R` | gene columns, expression lookup, alignment of the tables |
| `test_app_code.R` | the code rules listed above |

To add a test, add a `test_that_("what it checks", { ... })` block to the
file of that topic, or a new `tests/test_<topic>.R` listed in
`tests/run_tests.R`.

## The user manual

The manual is a [Sphinx](https://www.sphinx-doc.org/) project with Markdown
pages in `manual/docs/source/` (`user_manual.md`, `setup_guide.md`). The app
serves the HTML build in `manual/docs/build/html/` (the *User manual*
button), so rebuild and commit it after editing the pages:

```bash
pip install sphinx myst-parser sphinx-rtd-theme
sphinx-build -E -b html manual/docs/source manual/docs/build/html
```

## Code style

- Follow the style of the file you edit: R files in `functions/` use 4-space
  indentation, `ui/`, `ui.R` and `server.R` use 2 spaces.
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
