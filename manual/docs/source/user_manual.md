# User Manual

Welcome to PlaViSca, an interactive web application for exploring single-cell RNA-seq data from _Plasmodium vivax_. This manual provides comprehensive guidance on navigating and utilizing the application's features for malaria research.

**Quick Start:**
1. Open the **Filters** box on any tab to focus on relevant samples
2. Explore expression patterns and quality metrics via interactive visualizations
3. Compare multiple genes in the **Gene Expression** tab
4. Download plots and data for external analysis

**Data Integration**: PlaViSca brings together about 106,000 cells from six published studies, processed with one preprocessing pipeline. See the Data Processing Pipeline section for details.

## Overview of PlaViSca Features

PlaViSca is a single page: a top bar with the application name and the Institut Pasteur du Cambodge (IPC) and BAIA logos, and below it a row of tabs, each focusing on one aspect of the data:

- **Embeddings**
- **Gene Expression**
- **Plots**: charts of the cell metadata
- **Data table**: one row per cell with its metadata
- **QC Plots**
- **Data Sources**
- **Code**: opens the source code of the app on GitHub, in a new browser tab
- **Manual**: this manual
- **About**: the project, its team and how to get involved

The app opens on the Embeddings tab, over an animated particle background.

All data tabs (Embeddings, Gene Expression, Plots, Data table, QC Plots) share a common **Filters** row at the top with the following fields:

| Filter field | Description |
|---|---|
| Study label | Study the cell comes from |
| Broad life-cycle stage | Blood, liver or sporozoite stage |
| Life-cycle stage | Detailed stage (e.g. Ring/Early trophozoite, Trophozoite, Hypnozoite, Female gametocyte); only stages compatible with the selected broad stage are offered |
| Host species | Host organism |
| Host and tissue | Host blood, host liver or vector salivary gland |
| Treatment | Drug treatment; "Study control" is the control arm of a drug experiment, "No treatment" means the study had no drug arm |
| Parasite strain / isolate | Parasite strain or clinical isolate |
| Most similar reference time point (h) | For blood-stage cells: the time point of a reference blood-stage time course (Zhu et al. 2016) whose expression is most similar to the cell. It is a similarity, not a measured time |

Hover over the help icon next to a filter for its full description. Cells with a missing value are shown as "Missing"; for the gametocyte state, cells that are not gametocytes are shown as "No gametocyte".

Filters apply with **OR logic within each field** and **AND logic between fields**, allowing precise subsetting of the dataset. All tabs stay synchronized with each other's filter selections.

The **Filters** start folded, as a button with a funnel at the top left of the tab (above the cog of the plot settings). Click it to unfold the Filters above the plots, and click their title bar to fold them again. The number of filters set is shown on the button, or in the title, e.g. **Filters (2)**. Folding is shared by all tabs: folding the Filters on one tab folds them on every tab.

![Figure 1: the Filters box, unfolded](imgs/filters.png)
**Figure 1**: the Filters box, unfolded

### Embeddings

![Figure 2: Embeddings tab, with the plot settings open and cells colored by life-cycle stage](imgs/embeddings.png)
**Figure 2**: Embeddings tab, with the plot settings open and cells colored by life-cycle stage

![Figure 3: Compare Plots mode: the same embedding colored by broad life-cycle stage and by study](imgs/embeddings_compare.png)
**Figure 3**: Compare Plots mode: the same embedding colored by broad life-cycle stage and by study

The Embeddings tab offers dimensionality reduction and customizable visualization of single-cell data (**Figures 2 and 3**). Key capabilities:

**Plot Settings:** the settings sit in a panel left of the plot, folded by default: click the cog button to open it, and the arrow at its edge to fold it again. The Gene expression tab has the same panel, and the panel is open or folded on both tabs at once.

| Setting      | Options                                        |
|--------------|------------------------------------------------|
| Data Type    | Raw, Normalized (default: Normalized)          |
| Integration  | Integrated / Unintegrated (not available for t-SNE) |
| Embedding    | PCA, UMAP (default), t-SNE                     |
| Point Size   | S / M (default) / L                            |
| Dimension    | 2D / 3D (default)                              |
| X / Y axes   | Selectable when 2D mode is active              |

**Color by**: Study label, Broad life-cycle stage, Life-cycle stage, Host species, Host and tissue, Treatment, Parasite strain / isolate, Parasite origin location, Single-cell technology, or Genes.

**Visualization bar**: the title bar of the plot holds the **Compare Plots** and **Download Data** buttons.

**Compare Plots mode**: Click **Compare Plots** to display two independent plots (Plot A and Plot B) side-by-side, each with its own **Color by** selector in its title bar; a gene input appears above a plot when it is colored by genes.

**Interactive Cell Selection**: Click any cell to open a modal dialog showing a per-cell gene expression table (gene ID, gene name, expression value, description). The table is filterable and exportable as CSV.

**Download Data**: Exports the currently filtered metadata as a CSV file.

### Gene Expression

![Figure 4: Gene expression tab](imgs/gene_expression.png)
**Figure 4**: Gene expression tab

The Gene Expression tab provides an interactive interface for exploring gene expression across various biological conditions (**Figure 4**). Key features:

- **Flexible Plot Management**: Add or remove plot panels dynamically (panels display in a 2-column layout). Each panel has its own gene selection and grouping.
- **Data Type**: Choose Raw or Normalized expression data (Scaled data is currently disabled).
- **Gene Search**: Toggle between Gene Name and Gene ID search using the switch in each panel.
- **Gene Selection**: Select up to 10 genes per panel; multi-gene panels display the averaged expression.
- **Group by**: Organize expression by Study label, Broad life-cycle stage, Life-cycle stage, Host species, or Treatment.
- **Violin Plots**: Each panel displays a Plotly violin plot with hover tooltips showing mean, SD, and cell count per group.
- **Annotations**: Direct links to PlasmoDB and NCBI per gene (supports multiple tabs for multi-gene selections).
- **Download**: Export per-panel expression data as an `.xlsx` file containing an Expression Data sheet and a Metadata sheet.

### Plots

![Figure 5: Plots tab](imgs/plots.png)
**Figure 5**: Plots tab

The Plots tab shows how the selected cells are distributed over the metadata fields (**Figure 5**). Changing the filters updates the value boxes and all charts.

**Summary value boxes:**

| Box           | Description                            |
|---------------|----------------------------------------|
| Total Cells   | Number of cells after filtering        |
| Studies       | Number of distinct studies represented |
| Host Species  | Number of distinct host species        |

**Bar charts**, one per field: Broad life-cycle stage, Life-cycle stage, Host species, Parasite strain / isolate, Treatment, Most similar reference time point, Parasite origin location, Gametocyte call of the cluster, and Gametocyte sex (authors).

> **Note:** Blood-stage cells get the life-cycle stage inferred by PlaViSca: the gametocyte call of the cell's cluster, otherwise the most similar reference time point. Sporozoites and liver stages, which PlaViSca does not classify, keep the authors' labels. The field "Source of the life-cycle stage" in the Data table says which one a cell's stage comes from. The authors' gametocyte labels remain available for comparison ("Gametocyte sex (authors)").

Hover over a bar for details; use the controls in the top-right corner of a chart to zoom, pan or download it.

### Data table

![Figure 6: Data table tab](imgs/data_table.png)
**Figure 6**: Data table tab

One row per cell: the cell ID, study, SRA run and the authors' cell ID (missing for Hazzard2022, whose cells were called by PlaViSca), then the host and tissue, strain and origin, treatment, life-cycle stage (and the authors' stage), reference time point, gametocyte calls, biological replicate, day post infection, technology, sequencer and counts. Search globally or filter by column, and export the table as CSV.

### QC Plots

![Figure 7: QC Plots tab](imgs/quality_control.png)
**Figure 7**: QC Plots tab

The QC Plots tab provides visualizations to assess the integrity of single-cell RNA sequencing data (**Figure 7**).

**Summary value boxes:**

| Box               | Description                              |
|-------------------|------------------------------------------|
| Total Cells       | Number of cells after filtering          |
| Mean RNA Counts   | Mean number of RNA counts per cell       |
| Mean Features     | Mean number of features (genes) per cell |

**Plots:**

- **RNA Count distribution**: Violin plot of RNA counts per cell, per study, coloured by broad life-cycle stage.
- **Feature Count distribution**: Violin plot of feature (gene) counts per cell, per study, coloured by broad life-cycle stage.
- **Elbow Plot**: PCA standard deviation per component, per study — used to determine the optimal number of PCs.
- **Variable Genes**: Mean expression vs. standardized variance scatter plot, highlighting the top 15 highly variable features (HVF).

All plots respect the shared metadata filters and support interactive zooming, panning, and download via the Plotly toolbar.


### Data Sources

![Figure 8: Data sources tab](imgs/data_source.png)
**Figure 8**: Data sources tab

The Data Sources section provides detailed documentation and attribution for the datasets utilized within PlaViSca (**Figure 8**). It lists each study with its number of cells, authors, title, journal, year, PubMed ID and DOI link, enabling users to directly access the original publications. This ensures transparency, facilitates reproducibility, and appropriately credits the contributors of the datasets.

#### Data Processing Pipeline

All six studies are processed by the [PlaViSca preprocessing pipeline](https://github.com/baia-ipc/PlaViSca-meta-analysis): from the sequencing reads (STARsolo count matrices) or the authors' count matrices, through cell calling, normalisation, integration (Harmony), embeddings and life-cycle stage annotation, to the data files read by the app. Expression is available as raw counts and log-normalised values.

To set up the app with these files, see the [Setup Guide](setup_guide.md).

### About

The **About** tab describes what PlaViSca contains and how the data were processed, links to the source code of the app and of the analysis pipeline, and gives the citation of the PlaViSca study and the contact person.

## Best Practices

- **Filter First**: Start by setting the Filters to focus on relevant data.
- **Leverage Interactivity**: Hover for tooltips, download plots, and export data for external tools.
- **Quality Check**: Verify data reliability in the QC Plots tab before drawing conclusions.
- **Compare Systematically**: Add multiple Gene Expression plots side-by-side for cross-condition comparison.
- **Document Findings**: Download all plots and data for reproducibility.

## Contact

For questions, bug reports or suggestions, or if you would like a further dataset to be integrated, contact Giorgio Gonnella, Head of Bioinformatics and AI Applications, Institut Pasteur du Cambodge ([ggonnella@pasteur-kh.org](mailto:ggonnella@pasteur-kh.org)), or open an issue on [GitHub](https://github.com/baia-ipc/PlaViSca-app/issues).

PlaViSca is a research tool for exploring published data; verify results independently before drawing conclusions.
