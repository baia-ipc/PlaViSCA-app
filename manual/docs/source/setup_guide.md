# Setup Guide

This guide provides step-by-step instructions for installing and running PlaViSCA locally.

## System Requirements

- **Operating System**: macOS, Windows, or Linux
- **R Version**: 4.3.3 (tested) ([Download R](https://cran.r-project.org/))
- **RStudio**: 2024.09 or later (recommended) ([Download RStudio](https://posit.co/download/rstudio-desktop/))
- **Git**: 2.43 or later ([Download Git](https://git-scm.com/downloads))
- **RAM**: 8 GB minimum (one instance of the app uses about 2 GB with the normalized data, 3 GB once the raw data are loaded)
- **CPU**: 4 cores minimum
- **Disk Space**: 5 GB free

## Environment Snapshot (current)

The live app was run with the following versions:

- **R**: 4.3.3
- **renv**: 1.1.7
- **Core packages**: bslib (0.9.0), conflicted (1.2.0), DT (0.34.0), dplyr (1.1.4), fontawesome (0.5.3), later (1.4.5), Matrix (1.6-5), plotly (4.11.0), RColorBrewer (1.1-3), readr (2.1.6), shiny (1.12.1), shinycssloaders (1.1.0), shinyjs (2.1.1), stringr (1.6.0), tibble (3.3.1), tidyverse (2.0.0), viridis (0.6.5), writexl (1.5.4)

If you restore with `renv`, these exact versions will be installed automatically.

## Installation Steps

PlaViSCA is compatible with macOS, Windows, and Linux operating systems. Follow the steps below to set up the application on your system.

### 1. Clone the Repository

```bash
git clone https://github.com/baia-ipc/PlaViSCA-app.git
cd PlaViSCA-app
```

This will create a directory named `PlaViSCA-app` containing all application files.

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
  "RColorBrewer", "tidyverse", "later", "Matrix", "viridis",
  "readr", "tibble", "bslib", "fontawesome", "conflicted", "writexl"
))
```

#### Notes for macOS

`renv` installs ready-built packages where it can, and compiles the others
from source. Compiling needs Apple's command line tools:

```bash
xcode-select --install
```

The packages `openssl`, `curl` and `xml2` link to system libraries, and two
common setups make them fail to build or load:

- **conda or mamba on the `PATH`.** Their copies of these libraries get
  picked up, and the installed package then fails to load with
  `Library not loaded: @rpath/...`. Run `conda deactivate` (repeat until no
  environment is active), or remove conda from the `PATH` in that terminal,
  before `renv::restore()`.
- **Homebrew's OpenSSL 4.** The `openssl` package version used here builds
  against OpenSSL 3 (errors such as `incomplete definition of type
  'const ASN1_BIT_STRING'`). Install OpenSSL 3 and point the build at it:

  ```bash
  brew install openssl@3
  export PKG_CONFIG_PATH="$(brew --prefix openssl@3)/lib/pkgconfig"
  R -e 'renv::restore()'
  ```

`renv::restore()` installs all packages or none: after fixing a failed
package, run it again. Any R 4.3.x works; with a version other than 4.3.3,
R prints harmless warnings that packages were built under another version.

### 3. Prepare the Data

PlaViSCA reads its data from the directory named by the `PLAVISCA_DATA_DIR` environment variable (default: `data/` inside the app directory). This directory must contain:

| File | Description |
|---|---|
| `cleaned_dataset.rds` | Per-cell metadata and embeddings, per-study PCA and variable-gene tables |
| `normalized_expression.rds` | Log-normalised expression: a sparse matrix (`Matrix::dgCMatrix`), cells x genes, rows named by cell barcode in the order of the metadata, columns by gene ID |
| `raw_expression.rds` | Raw counts, same layout |
| `gene_annotation.rds` | _P. vivax_ P01 gene identifiers, names and descriptions |
| `data_source.csv` | Publication table for the Data Sources tab |

These files are written by the [PlaViSCA preprocessing pipeline](https://github.com/baia-ipc/PlaViSCA-meta-analysis) into the `results/app_data/` directory of a pipeline run. Point the app at them, for example:

```bash
export PLAVISCA_DATA_DIR=/path/to/run/results/app_data
```

At startup the app checks that the data match its configuration (studies, columns, life-cycle stages). If they do not, it stops with a message listing what is wrong.

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

1. Open RStudio and set the working directory to the `PlaViSCA-app` project folder.
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

## Serving Several Users

`./run_app.sh` is enough for one person, or for a few colleagues who take
turns. For a public server, read on.

### Why one R process is a bottleneck

A Shiny app runs in a single R process, and R does one thing at a time. All
browsers connected to that process share it: while it draws a plot for one
user, every other user waits, and the whole process holds one copy of the
data for everybody. The remedy is to run several copies of the app
(*instances*), each in its own R process, and to spread the users over them.

### Several instances behind nginx

Start one instance per port. Each one loads its own copy of the data, which
takes 2 to 3 GB of memory:

```bash
export PLAVISCA_DATA_DIR=/path/to/app_data
./run_app.sh --host 127.0.0.1 --port 3838 &
./run_app.sh --host 127.0.0.1 --port 3839 &
./run_app.sh --host 127.0.0.1 --port 3840 &
./run_app.sh --host 127.0.0.1 --port 3841 &
```

On a Linux server, let systemd start them at boot and restart them if they
stop. The repository provides a *template unit*, a service definition in
which `%i` stands for the instance name, here the port
([`deploy/systemd/plavisca@.service`](https://github.com/baia-ipc/PlaViSCA-app/blob/master/deploy/systemd/plavisca@.service)):

```ini
[Unit]
Description=PlaViSCA Shiny app on port %i
After=network.target

[Service]
Type=simple
User=plavisca
WorkingDirectory=/path/to/PlaViSCA-app
Environment=PLAVISCA_DATA_DIR=/path/to/app_data
ExecStart=/path/to/PlaViSCA-app/run_app.sh --host 127.0.0.1 --port %i
Restart=on-failure
RestartSec=10
TimeoutStartSec=300

[Install]
WantedBy=multi-user.target
```

`WorkingDirectory` must be the app directory: R reads the `.Rprofile`
there, which activates the renv library with the right package versions.
Install and start four instances with:

```bash
sudo cp deploy/systemd/plavisca@.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now plavisca@3838 plavisca@3839 plavisca@3840 plavisca@3841
journalctl -u plavisca@3838 -f     # follow the log of one instance
```

Then put [nginx](https://nginx.org/) in front as a *reverse proxy*: a web
server that receives the users on one address and forwards each of them to
one of the instances. Two things matter for Shiny:

- **Sticky sessions.** A Shiny session lives in one R process, so a browser
  must keep talking to the instance it started on. `ip_hash` sends each
  client address always to the same instance.
- **Websocket upgrade.** Shiny keeps one open connection (a *websocket*) per
  session, and the proxy must pass the `Upgrade` headers through.

A complete minimal server block
([`deploy/nginx/plavisca.conf`](https://github.com/baia-ipc/PlaViSCA-app/blob/master/deploy/nginx/plavisca.conf)):

```nginx
upstream plavisca {
    ip_hash;
    server 127.0.0.1:3838;
    server 127.0.0.1:3839;
    server 127.0.0.1:3840;
    server 127.0.0.1:3841;
}

server {
    listen 80;
    server_name plavisca.example.org;

    gzip on;
    gzip_types text/plain text/css application/javascript application/json
               image/svg+xml;
    gzip_min_length 1024;

    location / {
        proxy_pass http://plavisca;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
        proxy_buffering off;
    }
}
```

```bash
sudo cp deploy/nginx/plavisca.conf /etc/nginx/sites-available/plavisca
sudo ln -s /etc/nginx/sites-available/plavisca /etc/nginx/sites-enabled/
sudo nginx -t && sudo systemctl reload nginx
```

`gzip` compresses the page, scripts and styles sent over plain HTTP. The
plots travel over the websocket, and websocket messages are not compressed
by the proxy. For HTTPS, add a certificate (for example with
[certbot](https://certbot.eff.org/)) to the same server block.

### Alternatives

- **[ShinyProxy](https://www.shinyproxy.io/)** (open source) starts one
  Docker container, with its own R process, per user. The strongest
  isolation, at the cost of one data copy per user and a container start
  before the first page.
- **[Posit Connect](https://posit.co/products/enterprise/connect/)** and
  **Shiny Server Pro** (commercial) run several R processes per app and
  balance the users over them, with authentication built in.
- **[Shiny Server](https://posit.co/products/open-source/shiny-server/)**
  open source runs one R process per app, with no load balancing: all users
  of the app share that process, as with `./run_app.sh`. Simple to set up,
  fine for a small group.

### Sizing

- **Memory**: 2 to 3 GB per instance (each holds its own copy of the data:
  about 2 GB with the normalized data, 3 GB once the raw data are loaded),
  plus what the system needs. Check the instances with `free -h` once they
  are started.
- **CPU**: one core per instance plus one for the proxy and the system, so
  four instances need five cores.
- **Users per instance**: a handful; users are only served in turn while a
  plot is being drawn, so more instances mean shorter waits.

### Locale

Shiny Server, and systemd by default, run R in the C locale (plain ASCII,
no language settings). The app is written for that: its sources are ASCII
and it reads and writes its files as UTF-8, so no `LANG` or `LC_ALL`
setting is needed.

## Troubleshooting

| Problem | Solution |
|---|---|
| **Browser error on launch** (`'browser' must be a non-empty character string`) | R is trying to open a browser in a headless environment. Use `./run_app.sh` (browser disabled by default) or add `launch.browser = FALSE` to your `runApp()` call. |
| **`renv::restore()` fails** | Run `R -e "renv::status()"` to identify missing packages. On Linux, you may need system libraries: `sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev`. On macOS, see *Notes for macOS* in Step 2. |
| **App stops at startup with "data file(s) are missing"** | Set `PLAVISCA_DATA_DIR` to a directory containing the five files listed in Step 3. |
| **App stops at startup with "data check(s) failed"** | The data do not match `config/data_columns.yml` (e.g. a different number of cells or studies). Use the data written by the matching version of the preprocessing pipeline, or update `config/data_columns.yml` (see the developer guide, `docs/developer.md`). |
| **App is slow or crashes** | Check the memory (see the system requirements) and close other applications. Starting the app takes about 20 s (reading the data and drawing the first embedding plot); the raw data are read the first time a session selects them. If several people use the app at once, run several instances behind a proxy: see *Serving Several Users*. |
| **Browser display issues** | Clear the browser cache or try a different browser. Ensure JavaScript is enabled. |
| **Package not found after `renv::restore()`** | Try `renv::repair()` followed by `renv::restore()` again. |

For additional help, contact the development team or open a GitHub issue.

## Contact

For questions, feedback, or collaboration inquiries, please contact the corresponding authors:

- **Giorgio Gonnella**  
  _Head, Bioinformatics and Artificial Intelligence Unit (BAIA), Institut Pasteur du Cambodge_  
  [ggonnella@pasteur-kh.org](mailto:ggonnella@pasteur-kh.org)

- **Anthony Ruberto**  
  _Assistant Professor, Department of Biomedical Sciences, College of Medicine, Florida State University_  
  [anthony.ruberto@med.fsu.edu](mailto:anthony.ruberto@med.fsu.edu)

## Source Code

PlaViSCA is freely available under the Creative Commons Attribution 4.0 International license (CC BY 4.0). The authors maintain authority to update and improve the application. Developers and contributors are welcome to explore, modify, or extend the app by visiting the official GitHub repository:

[GitHub Repository – PlaViSCA](https://github.com/baia-ipc/PlaViSCA-app)

© 2025 BAIA Team, Institut Pasteur du Cambodge. All rights reserved.
