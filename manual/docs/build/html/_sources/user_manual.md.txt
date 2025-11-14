# User Manual

Welcome to PlaViSca, an interactive web application for exploring single-cell RNA-seq data from _Plasmodium vivax_. This manual provides comprehensive guidance on navigating and utilizing the application's features for malaria research.

**Data Sources**: PlaViSca integrates data from five studies processed using Nextflow pipelines for downloading, alignment, and feature counting. For details on data acquisition and processing, refer to the Installation section.

- Start with the Metadata tab to filter data relevant to your research.
- Use interactive plots to explore patterns; hover for details and download for reports.
- For gene expression, add multiple plots to compare genes across conditions.
- Check the Quality Control tab to ensure data reliability.
- Export data and plots for further analysis in external tools.

## Overview of PlaViSca Features

PlaViSca consists of three main sections: **Home page**, **Analysis page**, and **About page**. Each section is designed to offer an intuitive, code-free platform for exploring single-cell RNA-seq data from _Plasmodium vivax_.

## Home page

![Figure 2: PlaViSca home page](imgs/home_page.png)
**Figure 2**: PlaViSca home page

The Home page acts as the central landing point for users see **Figure 2**. It provides a concise overview of the application's purpose and offers direct access to its core functionalities.

**Menu bar**:

The menu bar provides consistent branding and navigation across the entire application. It is fixed at the top of the interface, ensuring it remains visible and accessible on all pages for easy navigation.

- Left side: Displays the Institut Pasteur logo, representing the institution supporting the project.
- Center: Shows the application name, PlaViSca, prominently, accompanied by a small animated mosquito icon that symbolizes the _Plasmodium vivax_ vector.
- Right side: Contains the main menu items, including Home, Analysis, and About, which allow users to navigate between different pages of the application. At the very end right of the bar, there are also feature icons representing the development and hosting units — Bioinformatics and Artificial Intelligence.

**Content**:

- The left section of the Home page contains a brief text outlining the goals and motivation behind the development of PlaViSca. It introduces the app as an interactive, code-free platform for exploring single-cell RNA-seq data from _P. vivax_, targeting researchers and public health professionals.
- The right section displays a grid of interactive icons, each linking to a specific part of the application. These icons act as shortcuts, enabling users to quickly access key modules:

- **Metadata**: Explore and filter dataset metadata.
- **Analysis**: Access the main analysis tabs (Embeddings, Quality Control, etc.).
- **Quality Control**: Assess data quality metrics.
- **Data Source**: View dataset references and sources.
- **Gene Expression**: Visualize gene expression patterns.
- **Contribution**: Learn how to contribute to the project.
- **User Manual**: Access this documentation.
- **Source Code**: View the GitHub repository.
- **About**: Information about the project and team.

Detailed explanations of these modules' functionalities are provided in the following sections.

**Footer**:

The footer features a mosquito animation alongside interactive particle effects, enhancing the visual appeal of the website and making the interface more dynamic and engaging. Additionally, the footer includes a copyright notice: © 2025 BAIA Team. All rights reserved. This acknowledges the work of the development team and reserves the rights to the content and design.

## Analysis page

The Analysis page is designed to facilitate interactive exploration of both metadata and gene expression data. The page is organized into five tabs, each focusing on a distinct aspect of data analysis:

### Metadata

<iframe width="100%" height="360" src="https://www.youtube.com/embed/znUUFwZDzY0" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

**Video 1:** Metadata filtration

A short video demonstration of the Metadata tab **Video 1**. This video provides an overview of how to filter datasets by biological and experimental attributes, such as host species, life cycle stage, treatment, parasite stage, and sample type. Applying filters dynamically updates both plots and tables, enabling users to focus on relevant data subsets for further analysis.

Interactive features include:

- **Plotly Charts**: Hover to view detailed values. Additional controls (zoom, pan, selection, download) are accessible in the chart's top-right corner.
- **DT Table**: Provides a global search box for keyword-based filtering and column-specific filters for precise data selection and export as a CSV file.

### Quality Control

![Figure 3: Quality control image](imgs/quality_control.png)
**Figure 3**: Quality control image

The Quality Control tab provides visualizations to assess the integrity of single-cell RNA sequencing data **Figure 3**, including:

- RNA Count vs. Sample
- nFeature Count vs. Sample
- Standard Deviation Plot of PCA
- Standard Variation vs. Mean Expression Plot

All plots support interactive filtering, enabling targeted quality assessment based on user-defined thresholds.

### Data source

![Figure 4: Data source](imgs/data_source.png)
**Figure 4**: Data source

The Data Source section provides detailed documentation and attribution for the datasets utilized within PlaViSca **Figure 4**. It includes comprehensive metadata such as journal names, publication dates, article titles, authors, and DOI links, enabling users to directly access the original publications. This ensures transparency, facilitates reproducibility, and appropriately credits the contributors of the datasets.

#### Data Processing Pipeline

PlaViSca integrates data from five studies processed using Nextflow pipelines for downloading, alignment, and feature counting.

- **Download FASTQ Files**: Scripts in the [download_SRA GitHub repo](https://github.com/Sopheap15/download_SRA) download data from NCBI. Follow the `README.md` for instructions.
- **Alignment and Feature Count**: Processed using STARsolo via Nextflow in the [sRNA_nextflow_starsolo GitHub repo](https://github.com/Sopheap15/sRNA_nextflow_starsolo).
- **Data Formats**: Gene expression data is provided in normalized, scaled, and cleaned formats. Metadata includes columns for study details, sample info, and biological annotations.

For technical details on data preparation, refer to the Installation section.

### Gene expression

<iframe width="100%" height="360" src="https://www.youtube.com/embed/ehDxOIu3fz0" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

**Video 2**: Gene expression tab functionality

The Gene Expression tab provides an interactive interface for exploring gene expression across various biological conditions **Video 2**. Users can:

- Add or remove plots as needed
- Select data type (Raw, Normalized, Scaled)
- Toggle between Gene Names and Gene IDs
- Search and select specific genes
- Group data by variables such as stage, species, or reproductive form

Each selection generates a dynamic violin plot with interactive filtering, hover tooltips, and download options. Direct links to PlasmoDB and NCBI facilitate access to gene annotations.

### Embedding

<iframe width="100%" height="360" src="https://www.youtube.com/embed/py5oW_ghmqY" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

**Video 3**: Embedding tab functionality

The Embedding tab offers tools for dimensionality reduction and customizable filtering of single-cell data **Video 3**. Users can:

- Apply multiple filters using OR logic within fields and AND logic between fields
- Color scatter plots by any metadata column or gene expression
- Select clustering-based coloring (CCA_sn) with adjustable resolution
- Choose data type and gene identifier for expression-based coloring
- Customize plot appearance (point size, 2D/3D, axis selection)
- Toggle between integrated and unintegrated datasets
- Select dimensionality reduction method (UMAP, t-SNE, PCA)
- Download plots for further use

#### Interactive cell selection

Clicking on individual cells displays a popup table with gene names, IDs, expression values, and descriptions. This data can be exported as CSV for downstream analysis.

## About page

![Figure 5: About page](imgs/about_page.png)
**Figure 5**: About page

The **About** page provides comprehensive information on the PlaViSca project, including its development background, team members, and how to get involved. It highlights the collaborative efforts behind creating a tool for malaria research.

### Project Background

PlaViSca was developed to democratize access to single-cell RNA-seq data for _Plasmodium vivax_, enabling researchers worldwide to explore parasite biology without coding expertise.

### Open-source and Collaboration

PlaViSca is an open-source initiative, and we welcome contributions from the scientific community. Researchers interested in:

- Integrating new datasets
- Enhancing existing features
- Reporting bugs or issues
- Collaborating on future development

... are warmly encouraged to get in touch with the development team.

## Tips for Effective Use

- Start with the Metadata tab to filter data relevant to your research.
- Use interactive plots to explore patterns; hover for details and download for reports.
- For gene expression, add multiple plots to compare genes across conditions.
- Check the Quality Control tab to ensure data reliability.
- Export data and plots for further analysis in external tools.

For technical setup, refer to the Installation section. If you encounter issues, consult the troubleshooting guide or contact the team.

## Support and Feedback

For questions, issues, or suggestions, contact the PlaViSca development team:

- **Emails**: osopheap@pasteur-kh.org, ggonnella@pasteur-kh.org
- **GitHub Issues**: [Report bugs or request features](https://github.com/Sopheap15/pvsca/issues)

© 2025 BAIA Team, Institut Pasteur du Cambodge. All rights reserved.

**Disclaimer**: This manual is provided "as is" for informational purposes. PlaViSca is a research tool; users should verify results independently.
