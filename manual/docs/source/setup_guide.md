# Setup Guide

<!-- <a href="https://github.com/Sopheap15/pvsca"><img alt="Version" src="https://img.shields.io/badge/version-1.0-blue?style=flat" /></a>
<img alt="Last Updated" src="https://img.shields.io/badge/last_updated-October%2021%2C%202025-blue?style=flat" />
<a href="https://github.com/Sopheap15/pvsca"><img alt="PlaViSca" src="https://img.shields.io/badge/PlaViSca-black?style=flat&logo=github" /></a> -->

This guide provides step-by-step instructions for installing and running PlaViSca locally. For online access, visit <a href="https://xxxx.com">PlaViSca Web Application</a>.

## System Requirements

- **Operating System**: macOS, Windows, or Linux
- **R Version**: 4.5 or later ([Download R](https://cran.r-project.org/))
- **RStudio**: 2025 or later (recommended) ([Download RStudio](https://posit.co/download/rstudio-desktop/))
- **Git**: 2.43 or later ([Download Git](https://git-scm.com/downloads))
- **RAM**: 16 GB minimum
- **CPU**: 4 cores minimum
- **Disk Space**: 5 GB free

## Installation Steps

PlaViSca is compatible with macOS, Windows, and Linux operating systems. Follow the steps below to set up the application on your system.

### 1. Clone the Repository

```bash
git clone https://github.com/Sopheap15/pvsca.git
```

This will create a directory named `PlaViSca` containing the application files.

### 2. Install Dependencies

Run the following command in the R console to install the required R packages:

```r
install.packages(c("shiny", "shinyjs", "shinycssloaders", "plotly", "DT", "RColorBrewer", "tidyverse", "later"))
```

### 3. Run the Application

- **If using RStudio (desktop):**

  - Click the "Run App" button in the top-right corner of the script editor.
  - Or run the following command in the R console:

  ```r
  shiny::runApp('.')
  ```

- **If using a Linux server (no GUI):**

  - Run the following command in your terminal to start the app and make it accessible from other machines:

  ```bash
  R -e "shiny::runApp('.', host='0.0.0.0', port=3838)"
  ```

  - Then, open a web browser on your local machine and go to: `http://<server-ip>:3838`
  - Replace `<server-ip>` with the actual IP address of your server. You can find it with:
    ```bash
    hostname -I
    ```

The application will launch in your default web browser (or be accessible remotely if running on a server).

## Troubleshooting

If you encounter issues during installation or running the app, refer to the following common problems and solutions:

- **App does not start**: Ensure all dependencies are installed correctly. Check that your R version is 4.5 or later. Try restarting RStudio.
- **Data not loading**: Verify that data files are placed in the `data/` directory with the correct names and formats (see User Manual for details).
- **Browser issues**: Clear your browser cache or try a different browser. Ensure JavaScript is enabled.
- **Performance issues**: PlaViSca requires at least 16 GB RAM. Close other applications if needed.
- **Package installation errors**: On Linux, you may need to install system libraries (e.g., `sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev`).

For additional help contact the development team.

## Development Team & Contact Information

For questions, feedback, or collaboration inquiries, please contact:

- **Sopheap Oeng**  
  _Bioinformatic System Engineer, Institut Pasteur du Cambodge_  
  📧 [osopheap@pasteur-kh.org](mailto:osopheap@pasteur-kh.org)

- **Brice Feufack Donfack**  
  _Postdoctoral researcher, Malaria Research Unit, Institut Pasteur du Cambodge_  
  📧 [idonfack@pasteur-kh.org](mailto:idonfack@pasteur-kh.org)

  - **Thomas Obdian**
    _, Institut Pasteur du French_  
    📧 [thomas.obadia@pasteur.fr](mailto:thomas.obadia@pasteur.fr)

- **Anthony Ruberto**  
  _Postdoctoral Researcher, Center for Tropical and Emerging Global Diseases, University of Georgia, USA_  
  📧 [aruberto@uga.edu](mailto:aruberto@uga.edu)

- **Giorgio Gonnella**  
  _Head, Bioinformatics and Artificial Intelligence Unit (BAIA), Institut Pasteur du Cambodge_  
  📧 [ggonnella@pasteur-kh.org](mailto:ggonnella@pasteur-kh.org)

## Source Code

PlaViSca is freely available under an open-source license. The authors maintain authority to update and improve the application. Developers and contributors are welcome to explore, modify, or extend the app by visiting the official GitHub repository:

🔗 [GitHub Repository – PlaViSca](https://github.com/Sopheap15/pvsca)

For more help, check the `README.md` in the repository or contact the developers.

© 2025 BAIA Team, Institut Pasteur du Cambodge. All rights reserved.
