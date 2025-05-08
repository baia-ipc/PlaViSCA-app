
# Plasmodium vivax Single Cell Analysis (PvSCA) Platform

**PvSCA** is an interactive, code-free R Shiny application designed to make  
single-cell RNA sequencing (scRNA-seq) data from *Plasmodium vivax* accessible  
to the broader scientific, public health, and educational communities.

## 🔍 Key Highlights

- Explore scRNA-seq data from *P. vivax* across different parasite and infection stages.
- Dynamic, interactive visualizations using Plotly (2D & 3D).
- Gene expression viewer for stage-specific or gene-specific profiling.
- Filter and subset data by condition, stage, species, and more.
- Embeddings viewer (PCA, UMAP, t-SNE) for both integrated and unintegrated datasets.
- Downloadable metadata and plots for downstream analysis.

---

## ✨ Features

### 🏠 Home
- Overview of PvSCA goals and applications.
- Interactive life cycle image map of *P. vivax*.

### ℹ️ About
- Information about the web application’s development and contributors.

### 📊 Explore Data
- Filter data by infection stage, parasite stage, coloring factor, gene, and more.
- Choose from raw, normalized, or scaled data.
- Toggle between 2D and 3D visualizations with customizable axes.
- Explore metadata in an interactive table with flexible filters.
- View integrated or unintegrated datasets using PCA, UMAP, or t-SNE embeddings.
- Access gene expression data with links to [PlasmoDB](https://plasmodb.org/) and [NCBI](https://www.ncbi.nlm.nih.gov/).

---

## 🚀 Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/Sopheap15/pvsca.git
```
After cloning the repository, the project directory will contain the following
structure:
.
├── app.R              # Main entry point, sources ui.R and server.R
├── data/              # Directory to store input datasets
├── functions/         # Custom functions and preprocessing scripts
├── global.R           # Loads data and functions into the global environment
├── pvsca.Rproj        # RStudio project file
├── README.md          # Project overview and installation instructions
├── ref/               # Reference data directory
├── renv/              # R environment directory managed by `renv`
├── renv.lock          # Snapshot of R packages for reproducibility
├── server.R           # Shiny server logic
├── ui.R               # Shiny user interface definition
└── www/               # Static assets (images, CSS, JavaScript)


### 2. Install Dependencies in RStudio

#### Option A: Use `renv`
```r
renv::restore()
```

#### Option B: Manual Installation
```r
install.packages(c(
  "shiny", "shinyjs", "shinycssloader", 
  "plotly", "DT", "RColorBrewer", 
  "tidyverse", "later"
))
```

---

### 3. Run the Application

#### Option A: From RStudio
- Open `app.R` (or `server.R` / `ui.R`) in RStudio.
- Click the **Run App** button in the top-right of the editor.

#### Option B: From the Command Line
```bash
R -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

---

#### 4. Options

The data used in this web application are derived from a flattened Seurat object.
A preprocessing pipeline is applied to clean and prepare the data before 
visualization. You can review the complete preprocessing workflow in 
functions/preprocessing.R.

---

## 📫 Feedback & Contributions

We welcome contributions and feedback from the community.  
Please open issues or submit pull requests on our [GitHub repository](https://github.com/Sopheap15/pvsca).

---

© 2025 BAIA Team. All rights reserved.