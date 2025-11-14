# PlaViSca features
PlaViSca is structured of three main components: **Home page**, **Analysis page**, and **About page**. Each section serves a distinct purpose in providing an interactive, user-friendly platform for exploring single-cell RNA-seq data from *Plasmodium vivax*.

## Home page
![Figure 2: PlaViSca home page](imgs/home_page.png)
**Figure 2**: PlaViSca home page

The Home page acts as the central landing point for users see **Figure 2**. It provides a concise overview of the application's purpose and offers direct access to its core functionalities.

**Menu bar**:

The menu bar provides consistent branding and navigation across the entire application. It is fixed at the top of the interface, ensuring it remains visible and accessible on all pages for easy navigation.
- Left side: Displays the Institut Pasteur logo, representing the institution supporting the project.
- Center: Shows the application name, PlaViSca, prominently, accompanied by a small animated mosquito icon that symbolizes the *Plasmodium vivax* vector.
- Right side: Contains the main menu items, including Home, Analysis, and About, which allow users to navigate between different pages of the application. At the very end right of the bar, there are also feature icons representing the development and hosting units — Bioinformatics and Artificial Intelligence.

**Content**:
- The left section of the Home page contains a brief text outlining the goals and motivation behind the development of PlaViSca. It introduces the app as an interactive, code-free platform for exploring single-cell RNA-seq data from *P. vivax*, targeting researchers and public health professionals.
- The right section displays a grid of interactive icons, each linking to a specific part of the application. These icons act as shortcuts, enabling users to quickly access key modules, including: Metadata, Analysis, Quality Control, Data Source, Gene Expression,Contribution, User Manual (this document), Source Code, About. 

A detailed explanation of these modules functionality is provided next page.

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

The **About** page provides background information on the development and vision behind the **PlaViSca** web application **Figure 5**. 

### Open-source and collaboration  
PlaViSca is an open-source initiative, and we welcome contributions from the scientific community. Researchers interested in:

- Integrating new datasets  
- Enhancing existing features  
- Reporting bugs or issues  
- Collaborating on future development  

... are warmly encouraged to get in touch with the development team.


### Development team & contact information

For questions, feedback, or collaboration inquiries, please contact:

- **Sopheap Oeng**  
  *Bioinformatic System Engineer, Institut Pasteur du Cambodge*  
  📧 [osopheap@pasteur-kh.org](mailto:osopheap@pasteur-kh.org)

- **Jean Popvici**  
  *Head, Malaria Research Unit, Institut Pasteur du Cambodge*  
  📧 [jpopovici@pasteur-kh.org](mailto:jpopovici@pasteur-kh.org)

- **Anthony Ruberto**  
  *Postdoctoral Researcher, Center for Tropical and Emerging Global Diseases, University of Georgia, USA*  
  📧 [aruberto@uga.edu](mailto:aruberto@uga.edu)

- **Giorgio Gonnella**  
  *Head, Bioinformatics and Artificial Intelligence Unit (BAIA), Institut Pasteur du Cambodge*  
  📧 [ggonnella@pasteur-kh.org](mailto:ggonnella@pasteur-kh.org)

---

### Source code  

PlaViSca is freely available under an open-source license. Developers and contributors can explore, modify, or extend the application by visiting the official GitHub repository:

🔗 [GitHub Repository – PlaViSca](https://github.com/Sopheap15/pvsca)
