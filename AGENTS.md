# PlaViSCA — Plasmodium vivax Single-Cell Analysis (Interactive Shiny App)

This document provides essential information for agentic coding agents operating in the PlaViSCA repository.

## Project Overview
PlaViSCA is an interactive R/Shiny application for exploring single-cell RNA-seq data from *Plasmodium vivax*. It enables researchers to visualize embeddings (PCA, UMAP, t-SNE), explore gene expression, and filter datasets interactively.

## Tech Stack
- **Language:** R (>= 4.3.3)
- **Framework:** Shiny
- **Package Management:** `renv`
- **Key Libraries:** `shinyjs`, `plotly`, `DT`, `tidyverse`, `memoise`, `later`

## Environment Setup & Commands

### Build & Dependencies
- **Restore dependencies:** `R -e "renv::restore()"`
- **Initialize renv (if needed):** `R -e "renv::init()"`

### Running the Application
- **Standard run:** `R -e "shiny::runApp('.')"`
- **Production run (background):** `R -e "shiny::runApp('.', host='0.0.0.0', port=3838)" &`

### Linting & Formatting
The project uses `styler` and `lintr` (available in `renv.lock` suggestions).
- **Format all files:** `R -e "styler::style_dir()"`
- **Lint the project:** `R -e "lintr::lint_dir()"`

### Testing
Currently, no formal test suite is implemented. If adding tests, use `testthat`.
- **Run all tests (standard convention):** `R -e "testthat::test_dir('tests')"`
- **Run a single test file:** `R -e "testthat::test_file('tests/testthat/test-feature.R')"`

## Code Style & Guidelines

### General R Style
- **Assignment:** Always use `<-` for assignment (avoid `=`).
- **Piping:** Use the `magrittr` pipe `%>%` for readable data transformations.
- **Indentation:** Use 4 spaces for indentation.
- **Comments:** Use `#` for comments. Describe the *why* of complex logic. Use section headers for large files.

### Naming Conventions
- **Functions:** Use `snake_case` (e.g., `make_cache_key`, `sync_data_type`).
- **Variables:** Use `snake_case` (e.g., `norm_df`, `filtered_meta`).
- **Constants:** Use `SCREAMING_SNAKE_CASE` (e.g., `DATA_PATHS`).
- **UI Elements:** Input/Output IDs should be descriptive `snake_case` (e.g., `genes_exp_0`, `apply_filter`).

### Shiny Development Patterns
- **Modularity:** Keep `ui.R` and `server.R` clean. Move logic to the `functions/` directory.
- **Reactivity:** 
    - Use `req()` to ensure inputs are available before running reactive logic.
    - Use `observeEvent()` for side effects (e.g., UI updates, `shinyjs` calls).
    - Use `reactive()` or `eventReactive()` for data calculations.
- **Performance:** 
    - Use `memoise` for expensive data loading.
    - Use `debounce()` for inputs that trigger heavy computations or plots (e.g., search bars).
    - Leverage `sessionData` (an environment) for caching results within a session.
- **Error Handling:** Wrap risky operations in `tryCatch()` to prevent app crashes. Provide silent fallbacks or user notifications where appropriate.

### Best Practices & Pitfalls
- **Avoid Large Copies:** When filtering large datasets, avoid making full copies of the data matrix. Store row indices and subset only when necessary for plotting.
- **Resource Management:** Call `gc()` (garbage collection) after removing large objects from memory or switching datasets.
- **Namespace Safety:** While the app uses `library()`, prefer `pkg::function()` for clarity in helper functions (e.g., `dplyr::filter()`).
- **Input Validation:** Always check if `input$var` is `NULL` or empty before using it in a query or filter.
- **UI Responsiveness:** Use `withSpinner()` from `shinycssloaders` for long-running plot renders to provide visual feedback.

### Imports & Dependencies
- Package dependencies are managed in `renv.lock`.
- Shared logic is sourced in `global.R` via `source("functions/filename.R")`.
- Always verify if a package is in `renv.lock` before adding a new `library()` call.

## Repository Structure
- `app.R`: Entry point (loads `ui.R` and `server.R`).
- `ui.R` / `server.R`: Shiny UI and Server logic.
- `global.R`: Global variables, library loads, and sourcing functions.
- `functions/`: Core logic, helpers, and plotting scripts.
    - `embedding_plots.R`: Logic for generating Plotly embeddings.
    - `helpers_server.R`: Server-side helper functions.
    - `helpers_UI.R`: UI-side helper functions and modular components.
- `data/`: RDS files for app data (managed separately).
- `ref/`: Reference data (e.g., GFF annotations).
- `www/`: Static assets (CSS, JS, images).
- `pre_process_data/`: Scripts and temporary data for preprocessing Seurat objects.
- `manual/`: Documentation source and built HTML files.

## Data Management & State
- **Data Storage:** Most data is stored as `.rds` files. Use `readRDS()` to load them.
- **Preprocessing:** The `pre_process_data/` directory contains scripts used to convert Seurat objects into the slim RDS files used by the app.
- **Session State:** Use a dedicated environment (e.g., `sessionData <- new.env()`) for session-specific caching to avoid re-reading files from disk.

## CSS & JavaScript Integration
- **Static Assets:** Located in `www/`. Includes `style.css`, `script.js`, and specialized libraries like `particles.min.js`.
- **ShinyJS:** Heavily used for UI manipulations (hiding/showing sections, running custom JS).
- **Custom Scripts:** `www/script.js` handles client-side interactions and should be updated for complex UI behaviors that don't require server round-trips.

## Common Development Workflows
1. **Adding a Helper:** Add the R function to `functions/`, then source it in `global.R`.
2. **Adding a UI Component:** Update `ui.R` using the modular patterns (e.g., `create_meta_filter_row`).
3. **Updating Data:** Place new `.rds` files in `data/` and update `DATA_PATHS` in `global.R`.
4. **Modifying Styles:** Edit `www/style.css` for visual changes.

## Cursor & Copilot Rules
(No project-specific `.cursorrules` or `.github/copilot-instructions.md` were found. Follow the guidelines above as the primary source of truth.)
