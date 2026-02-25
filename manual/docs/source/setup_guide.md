# Setup Guide

This guide provides step-by-step instructions for installing and running PlaViSca locally. For online access, visit <a href="https://xxxx.com">PlaViSca Web Application</a>.

## System Requirements

- **Operating System**: macOS, Windows, or Linux
- **R Version**: 4.3.3 (tested) ([Download R](https://cran.r-project.org/))
- **RStudio**: 2024.09 or later (recommended) ([Download RStudio](https://posit.co/download/rstudio-desktop/))
- **Git**: 2.43 or later ([Download Git](https://git-scm.com/downloads))
- **RAM**: 16 GB minimum
- **CPU**: 4 cores minimum
- **Disk Space**: 5 GB free

## Environment Snapshot (current)

The live app was run with the following versions:

- **R**: 4.3.3
- **renv**: 1.1.7
- **Core packages**: bslib (0.9.0), conflicted (1.2.0), DT (0.34.0), dplyr (1.1.4), fontawesome (0.5.3), later (1.4.5), memoise (2.0.1), plotly (4.11.0), RColorBrewer (1.1-3), readr (2.1.6), shiny (1.12.1), shinycssloaders (1.1.0), shinyjs (2.1.1), stringr (1.6.0), tibble (3.3.1), tidyverse (2.0.0), viridis (0.6.5), writexl (1.5.4)

If you restore with `renv`, these exact versions will be installed automatically.

## Installation Steps

PlaViSca is compatible with macOS, Windows, and Linux operating systems. Follow the steps below to set up the application on your system.

### 1. Clone the Repository

```bash
git clone https://github.com/Sopheap15/PlaViSca.git
cd PlaViSca
```

This will create a directory named `PlaViSca` containing all application files.

### 2. Install Dependencies

**Recommended — restore exact versions with `renv`:**

```r
install.packages("renv")
renv::restore()
```

This installs the exact package versions listed above. If you prefer a minimal install without `renv`, use:

```r
install.packages(c(
  "shiny", "shinyjs", "shinycssloaders", "plotly", "DT",
  "RColorBrewer", "tidyverse", "later", "memoise", "viridis",
  "readr", "tibble", "bslib", "fontawesome", "conflicted", "writexl"
))
```

### 3. Prepare the Data

PlaViSca expects the following preprocessed data files in the `data/` directory:

| File | Description |
|---|---|
| `normalize_df.rds` | Normalized expression + metadata + embeddings |
| `raw_df.rds` | Raw counts + metadata + embeddings |
| `cleaned_dataset.rds` | Metadata, per-study PCA std dev, and HVF data |
| `data_source.csv` | Publication table for the Data Sources tab |

And a reference annotation file in `ref/`:

| File | Description |
|---|---|
| `PvivaxP01_gff_data.rds` | _P. vivax_ P01 gene annotations (gene ID, name, description) |

If you need to generate these files from raw data, follow the preprocessing pipeline in the companion repository: [PlaViSca_pre_process](https://github.com/Sopheap15/PlaViSca_pre_process).

### 4. Run the Application

#### Option A: Using the helper script (Linux/macOS — Recommended)

A bash script is provided to simplify running the app, especially on servers.

```bash
chmod +x run_app.sh
./run_app.sh
```

To expose the app on a specific host and port:

```bash
./run_app.sh --host 0.0.0.0 --port 3838
```

**Available options:**

| Flag | Description | Default |
|---|---|---|
| `-h, --host` | Host IP address | `0.0.0.0` |
| `-p, --port` | Port number | `3838` |
| `-b, --browser` | Auto-open browser on launch | disabled |
| `--help` | Show usage information | — |

Then open a browser and go to: `http://localhost:3838` (or replace `localhost` with your server IP).

#### Option B: Using RStudio (Desktop)

1. Open RStudio and set the working directory to the `PlaViSca` project folder.
2. Open `app.R` in the editor.
3. Click the **Run App** button in the top-right corner of the editor pane.

The app will launch in your default web browser.

#### Option C: From the terminal (R command)

```bash
R -e "shiny::runApp('.', launch.browser = TRUE)"
```

For a server without a display:

```bash
R -e "shiny::runApp('.', host='0.0.0.0', port=3838, launch.browser=FALSE)"
```

Then open `http://<server-ip>:3838` in a browser. Find your server IP with:

```bash
hostname -I
```

## Troubleshooting

| Problem | Solution |
|---|---|
| **Browser error on launch** (`'browser' must be a non-empty character string`) | R is trying to open a browser in a headless environment. Use `./run_app.sh` (browser disabled by default) or add `launch.browser = FALSE` to your `runApp()` call. |
| **`renv::restore()` fails** | Run `R -e "renv::status()"` to identify missing packages. On Linux, you may need system libraries: `sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev` |
| **Data not loading** | Verify that all required files are in `data/` and `ref/` with the exact names listed in Step 3. |
| **App is slow or crashes** | PlaViSca requires at least 16 GB RAM. Close other applications. The first load is slower due to data caching. |
| **Browser display issues** | Clear the browser cache or try a different browser. Ensure JavaScript is enabled. |
| **Package not found after `renv::restore()`** | Try `renv::repair()` followed by `renv::restore()` again. |

For additional help, contact the development team or open a GitHub issue.

## Development Team & Contact Information

For questions, feedback, or collaboration inquiries, please contact:

- **Sopheap Oeng**  
  _Bioinformatic System Engineer, Institut Pasteur du Cambodge_  
  [osopheap@pasteur-kh.org](mailto:osopheap@pasteur-kh.org)

- **Lionel Brice Feufack-Donfack**  
  _Postdoctoral Researcher, Malaria Research Unit, Institut Pasteur du Cambodge_  
  [ldonfack@pasteur-kh.org](mailto:ldonfack@pasteur-kh.org)

- **Thomas Obadia**  
  _Senior Scientist, Institut Pasteur du Cambodge_  
  [tobadia@pasteur-kh.org](mailto:tobadia@pasteur-kh.org)

- **Anthony Ruberto**  
  _Senior Scientist, Institut Pasteur du Cambodge_  
  [aruberto02@gmail.com](mailto:aruberto02@gmail.com)

- **Giorgio Gonnella**  
  _Head, Bioinformatics and Artificial Intelligence Unit (BAIA), Institut Pasteur du Cambodge_  
  [ggonnella@pasteur-kh.org](mailto:ggonnella@pasteur-kh.org)

## Source Code

PlaViSca is freely available under an open-source license. The authors maintain authority to update and improve the application. Developers and contributors are welcome to explore, modify, or extend the app by visiting the official GitHub repository:

[GitHub Repository – PlaViSca](https://github.com/Sopheap15/PlaViSca)

© 2025 BAIA Team, Institut Pasteur du Cambodge. All rights reserved.
