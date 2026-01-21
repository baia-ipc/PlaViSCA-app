#!/usr/bin/env bash
set -e

echo "=== Welcome PlaViSca web app ==="
echo "Checking system requirements..."

# -------- Check R availability --------
if ! command -v Rscript >/dev/null 2>&1; then
  echo "✗ Rscript not found. Please install R."
  exit 1
fi

# -------- Check required R packages --------
echo "Checking required R packages..."

Rscript - <<'EOF'
required_packages <- c(
  "shiny", "shinyjs", "plotly", "DT", "tidyverse",
  "memoise", "later", "RColorBrewer"
)

missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("✓ All required packages available\n")
}
EOF

# -------- Check data directory --------
if [[ ! -d "data" ]]; then
  echo "✗ Data directory not found"
  exit 1
else
  echo "✓ Data directory found"
fi

# -------- Check helper functions --------
if [[ ! -f "functions/helpers_server.R" ]]; then
  echo "✗ Helper functions not found"
  exit 1
else
  echo "✓ Helper functions available"
fi

# -------- Create cache directory --------
if [[ ! -d "cache" ]]; then
  mkdir -p cache
  echo "✓ Created cache directory for performance optimization"
else
  echo "✓ Cache directory exists"
fi

echo
echo "=== Starting PlaViSca ==="
echo "Application will be available at: http://localhost:3838"
echo "Press Ctrl+c to stop the application"
echo "First load may take longer ..."
echo

# -------- Run Shiny app --------
Rscript - <<'EOF'
suppressMessages(library(shiny))

runApp(
  appDir = ".",
  host = "0.0.0.0",
  port = 3838,
  launch.browser = FALSE
)
EOF
